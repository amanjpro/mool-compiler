/*
 * Mool Compiler, is a toy compiler written in Scala, which compiles programs
 * written in Mool to Java bytecode
 * Copyright (C) <2012-2013>  Amanj Sherwany <http://wwww.amanj.me>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package ch.usi.inf.l3.moolc.codegen

import _root_.ch.usi.inf.l3.moolc.ir._

import org.objectweb.asm._
import org.objectweb.asm.commons._
import org.objectweb.asm.Opcodes._


class BytecodeGenerator(pgm: IRProgram, dir: String) {
	
	var isInit = false
	def start() = writeProgram
	
	private def writeProgram() = {
		pgm.classes.foreach(writeClass(_))
	}
	
	private def writeClass(clazz: IRClass) = {
		val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
		val directParent = if(clazz.name.contains("_")){
			clazz.name.substring(0, clazz.name.indexOf("_"))
			} else "java/lang/Object"
		cw.visit(V1_5, ACC_PUBLIC, clazz.name, null, directParent, null)
		clazz.vars.foreach(writeField(cw, _))
		writeConstructor(cw, clazz.args, clazz, clazz.const)
		clazz.methods.foreach(writeMethod(cw, _, clazz))
		val code = cw.toByteArray()
		
		val fos = new java.io.FileOutputStream(dir + "/" + clazz.name+".class")
		fos.write(code)
		fos.close
	}
	
	private def writeConstructor(cw: ClassWriter, 
																			args: List[IRVar], clazz: IRClass,
																							exprs: List[IRExpr]) = {
		
		isInit = true
		val mw = cw.visitMethod(ACC_PUBLIC, "<init>", 
							getBytecodeInitType(args), null, null)
		mw.visitCode();
		val start = new Label
		val end = new Label
		val map = prepareInit(args, exprs)
		mw.visitLabel(start)
		//pushes the 'this' variable
		mw.visitVarInsn(ALOAD, 0)
		//invokes the super class constructor
		mw.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", 
													"<init>", "()V")
		// writeParams(mv, method.args, map, start, end, isMain)
		for(expr <- exprs){
			writeInstruction(clazz, mw, expr, map, start, 
									end, clazz.vars ::: exprs ::: args)
		}
		mw.visitInsn(RETURN)
		mw.visitMaxs(1, 1)
		mw.visitLabel(end)
		mw.visitEnd
		isInit = false
	}
	
	private def writeMethod(cw: ClassWriter, method: IRMethod, clazz: IRClass) = {
		val isMain = (method.name == "main" && method.args == Nil 
												&& method.tpe == IRVoid && method.isStatic)
		val mv = isMain match {
			case true => 
				//main method
        cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
                "([Ljava/lang/String;)V", null, null)
			case false => 
				val acc_id = method.isStatic match{
					case true => ACC_PUBLIC + ACC_STATIC
					case false => ACC_PUBLIC
				}
				cw.visitMethod(acc_id, method.name, 
									getBytecodeMethodType(method.args, method.tpe), null, null)
		}
		val start = new Label
		val end = new Label
		val map = prepareMethod(method, isMain)
		mv.visitLabel(start)
		// writeParams(mv, method.args, map, start, end, isMain)
		for(expr <- method.body){
			writeInstruction(clazz, mv, expr, map, start, 
									end, clazz.vars ::: method.body ::: method.args)
		}
		if(method.tpe == IRVoid) mv.visitInsn(RETURN)
		else mv.visitInsn(getReturnBytecode(method))
		mv.visitMaxs(1, 1)
		mv.visitLabel(end)
		mv.visitEnd
	}
		
	private def writeField(cw: ClassWriter, v: IRVar) = {
		val default = v.tpe match{
			case IRStr => null
			case IRObject(x) => null
			case _ => 0
		}
		cw.visitField(ACC_PUBLIC, v.name, getBytecodeType(v.tpe), null,
				 default).visitEnd()
 	}
	
	// private def writeParams(mv: MethodVisitor, params: List[IRVar]
	// 							, map: Map[IRVar, Int], start: Label, end: Label
	// 							, isMain: Boolean) = {
	// 		if(!isMain){
	// 			for(param <- params) {
	// 				mv.visitLocalVariable(param.name, getBytecodeType(param.tpe), 
	// 												null, start, end, map(param))
	// 			}
	// 		}
	// 		else
	// 			mv.visitLocalVariable("args", "[Ljava/lang/String;", null, start, end, 0)
	// 	}
	
	private def prepareInit(args: List[IRVar], 
														exprs: List[IRExpr]): Map[IRVar, Int]={
		var i = 0
		var map: Map[IRVar, Int] = Map.empty
		for(arg <- args) {
			i = i + 1
			map = map + (arg -> i)
		}
		for(v <- exprs) {
			if(v.isInstanceOf[IRVar]){
				i = i + 1
				map = map + (v.asInstanceOf[IRVar] -> i)
			}
		}
		map
	}
	private def prepareMethod(method: IRMethod,
			 														isMain: Boolean): Map[IRVar, Int] = {
		var i = if(isMain) 0 else if(method.isStatic) -1 else 0
		var map: Map[IRVar, Int] = Map.empty
		for(arg <- method.args) {
			i = i + 1
			map = map + (arg -> i)
		}
		for(v <- method.body) {
			if(v.isInstanceOf[IRVar]){
				i = i + 1
				map = map + (v.asInstanceOf[IRVar] -> i)
			}
		}
		map
	}
	
	private def writeInstruction(clazz: IRClass, mw: MethodVisitor, expr: IRExpr, 
						map: Map[IRVar, Int], start: Label, end: Label, 
										exrps: List[IRExpr]): Unit = {
		expr match {
			case IRConst(v) => 
				//pushes constant v
				v match {
					case IRIntValue(x) => 
						mw.visitIntInsn(SIPUSH, x);
					case IRBoolValue(x) => 
						x match {
							case true => mw.visitInsn(ICONST_1)
							case false => mw.visitInsn(ICONST_0)
						}
					case IRStringValue(x) => mw.visitLdcInsn(x)
					case IRNullValue => mw.visitInsn(ACONST_NULL)
				}
				
			case IRBinary(e1, e2, op, e3) =>
				if(clazz.vars.contains(e1) && !isInit)
					mw.visitVarInsn(ALOAD, 0)
				(op, e2.tpe, e1.tpe) match {
					case (IROp.Add, IRStr, _) | (IROp.Add, _, IRStr) =>
						getBytecodeStrConcat(clazz, mw, e2, e3, map)
					case (IROp.Add, _, _) 
							| (IROp.Sub, _, _) 
							| (IROp.Div, _, _)
							| (IROp.Mul, _, _)
							| (IROp.Mod, _, _) => 
						loadVar(clazz, mw, e2, map)
						loadVar(clazz, mw, e3, map)
						mw.visitInsn(getBytecodeOperation(op, e2.tpe, e3.tpe))
					case _ => 
						loadVar(clazz, mw, e2, map)
						loadVar(clazz, mw, e3, map)
						getBytecodeLogicOp(mw, e2, e3, op)
				}
				storeVar(clazz, mw, e1, map)
			case IRAssignVar(e, v) =>
				if(clazz.vars.contains(e) && !isInit)
					mw.visitVarInsn(ALOAD, 0)
				loadVar(clazz, mw, v, map)
				storeVar(clazz, mw, e, map)
			case IRAssignConst(e, c) =>
				if(clazz.vars.contains(e) && !isInit)
					mw.visitVarInsn(ALOAD, 0)
				writeInstruction(clazz, mw, c, map, start, end, exrps)
				storeVar(clazz, mw, e, map)
			case IRAssignCall(e, c) => 
				if(clazz.vars.contains(e) && !isInit)
					mw.visitVarInsn(ALOAD, 0)
				writeInstruction(clazz, mw, c, map, start, end, exrps)
				storeVar(clazz, mw, e, map)
			case IRAssignNew(e, n) => 
				if(clazz.vars.contains(e) && !isInit)
					mw.visitVarInsn(ALOAD, 0)
				writeInstruction(clazz, mw, n, map, start, end, exrps)
				storeVar(clazz, mw, e, map)
			case v @ IRVar(n, tpe) =>
				mw.visitLocalVariable(n, getBytecodeType(tpe), null, start, end, map(v))
			case IRNotVar(v) => //TODO
			case IRLabel(l) => mw.visitLabel(l)
			case IRIF(c, g) =>
				loadVar(clazz, mw, c.value, map)
				mw.visitJumpInsn(IFEQ, g.l)
			case IRWhileIf(c, g) =>
				loadVar(clazz, mw, c, map)
				mw.visitJumpInsn(IFNE, g.l)
			case IRGoto(l) => 
				mw.visitInsn(ICONST_1)
				mw.visitInsn(ICONST_0)
				mw.visitJumpInsn(IF_ICMPNE, l.l)
			case IRPrint(v) =>
				//pushes the 'out' field (of type PrintStream) of the System class
				mw.visitFieldInsn(GETSTATIC, "java/lang/System", "out",
										"Ljava/io/PrintStream;")
				loadVar(clazz, mw, v, map)
				//invokes the 'println' method (defined in the PrintStream class)
				mw.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println",
									"(Ljava/lang/String;)V");
			case IRCall(c, m, args, s) =>
				val caller = s match{
					case true => c
					case false => 
						if(c == "this"){
							mw.visitVarInsn(ALOAD, 0)
							clazz.name
						}
						else {
							val tpe = findVarType(exrps, c).asInstanceOf[IRObject]
							loadVar(clazz, mw, IRVar(c, tpe), map)
							tpe.clazz
						}
				}
				val method = pgm.getClass(caller).get.getMethod(m).get
				val tpe = getBytecodeMethodType(method.args, method.tpe)
				val invoke = if(method.isStatic) INVOKESTATIC else INVOKEVIRTUAL
				for(arg <- args) loadVar(clazz, mw, arg, map)
				mw.visitMethodInsn(invoke, caller, method.name, tpe)
			case IRInvoke(c, m, args) => 
				writeInvoke(c, m, args, mw, map, clazz)
			case IRNew(c, args) =>
				mw.visitTypeInsn(NEW, c)
				mw.visitInsn(DUP)
				val cc = pgm.getClass(c).get
				val tpe = getBytecodeInitType(cc.args)
				for(arg <- args) loadVar(clazz, mw, arg, map)
				mw.visitMethodInsn(INVOKESPECIAL, c, "<init>", tpe)
			case IRReturn(v) => loadVar(clazz, mw, v, map)
		}
	}
	
	private def getBytecodeLogicOp(mv: MethodVisitor, v1: IRVar, v2: IRVar,
							op: IROp.Value) = {
		val l2 = new Label()
		mv.visitJumpInsn(getLogicOperator(op, v1), l2)
		mv.visitInsn(ICONST_1)
		val l3 = new Label()
		mv.visitJumpInsn(GOTO, l3);
		mv.visitLabel(l2)
		mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null)
		mv.visitInsn(ICONST_0);
		mv.visitLabel(l3);
		mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, 
										Array[java.lang.Object](Opcodes.INTEGER))
	}
	
	
	private def getLogicOperator(op: IROp.Value, v1: IRVar) = {
		op match {
			case IROp.Eq if(v1.tpe == IRInt || v1.tpe == IRBool)=> IF_ICMPNE
			case IROp.Eq => IF_ACMPNE
			case IROp.Neq if(v1.tpe == IRInt || v1.tpe == IRBool)=> IF_ICMPEQ
			case IROp.Neq => IF_ACMPEQ
			case IROp.Gt => IF_ICMPLE
			case IROp.Lt => IF_ICMPGE
			case _ => throw new Error("Never happens")
		}
	}
	private def getLogicOpcode(tpe: IRType): java.lang.Object = {
		tpe match{
			case IRInt => INTEGER
			case IRBool => INTEGER
			case IRObject(x) => x
			case IRStr => "java/lang/String"
			case _ => throw new Error("Never happens")
		}
	}
	private def loadVar(clazz: IRClass, mw: MethodVisitor, 
									v: IRVar, map: Map[IRVar, Int]) = {
		clazz.vars.contains(v) match{
			case true => 
				if(!isInit)
					mw.visitVarInsn(ALOAD, 0)
				mw.visitFieldInsn(GETFIELD, clazz.name, v.name, 
																		getBytecodeType(v.tpe))
			case false => 
				if(isInit)
					mw.visitVarInsn(ALOAD, 0)
				mw.visitVarInsn(getBytecodeLoad(v.tpe), map(v))
		}
	}
	
	private def storeVar(clazz: IRClass, mw: MethodVisitor, 
									v: IRVar, map: Map[IRVar, Int]) = {
		clazz.vars.contains(v) match{
			case true => 
				mw.visitFieldInsn(PUTFIELD, clazz.name, v.name, 
																		getBytecodeType(v.tpe))
			case false => mw.visitVarInsn(getBytecodeStore(v.tpe), map(v))
		}							
	}
	private def getBytecodeLoad(tpe: IRType) = {
		tpe match {
			case IRInt => ILOAD
			case IRBool => ILOAD
			case IRStr => ALOAD
			case IRObject(x) => ALOAD
			case _ => throw new Error("Impossible type")
		}
	}
	
	private def writeInvoke(c: IRVar, m: IRVar, args: List[IRVar],
					mv: MethodVisitor, map: Map[IRVar, Int], clazz: IRClass){
		val l0 = new Label();
		val l1 = new Label();
		val l2 = new Label();
		mv.visitTryCatchBlock(l0, l1, l2, "java/lang/ClassNotFoundException");
		val l3 = new Label();
		mv.visitTryCatchBlock(l0, l1, l3, "java/lang/NoSuchMethodException");
		val l4 = new Label();
		mv.visitTryCatchBlock(l0, l1, l4, "java/lang/IllegalAccessException");
		val l5 = new Label();
		mv.visitTryCatchBlock(l0, l1, l5, "java/lang/reflect/InvocationTargetException");
		mv.visitLabel(l0);
		//load the variable that contains the class name
		loadVar(clazz, mv, c, map) 
		mv.visitMethodInsn(INVOKESTATIC, "java/lang/Class", "forName", 
																			"(Ljava/lang/String;)Ljava/lang/Class;");
		//load the variable that contains the method name
		loadVar(clazz, mv, m, map)
		//the size of the array of args class types
		mv.visitIntInsn(SIPUSH, args.length);
		mv.visitTypeInsn(ANEWARRAY, "java/lang/Class");
		var i = 0
		var temp = args
		//write the types of the parameters
		while(temp != Nil){
			var head = temp.head
			mv.visitInsn(DUP);
			mv.visitIntInsn(SIPUSH, i);
			head.tpe match{
				case IRInt => 
					mv.visitFieldInsn(GETSTATIC, "java/lang/Integer", 
																									"TYPE", "Ljava/lang/Class;");
				case IRBool => 
					mv.visitFieldInsn(GETSTATIC, "java/lang/Boolean", 
																									"TYPE", "Ljava/lang/Class;");
				case IRStr =>
					mv.visitLdcInsn("java.lang.String");
					mv.visitMethodInsn(INVOKESTATIC, "java/lang/Class", "forName", 
																			"(Ljava/lang/String;)Ljava/lang/Class;");
				case IRObject(x) =>
					mv.visitLdcInsn(x);
					mv.visitMethodInsn(INVOKESTATIC, "java/lang/Class", "forName", 
																		"(Ljava/lang/String;)Ljava/lang/Class;");
				case _ => 
			}
			mv.visitInsn(AASTORE);
			i += 1
			temp = temp.tail
		}
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getDeclaredMethod", 
						"(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;");
		
		mv.visitInsn(ACONST_NULL);
		mv.visitIntInsn(SIPUSH, args.length);
		mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
		i = 0
		temp = args
		while(temp != Nil){
			var head = temp.head
			mv.visitInsn(DUP);
			mv.visitIntInsn(SIPUSH, i)
			loadVar(clazz, mv, head, map)
			head.tpe match{
				case IRInt => 
					mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", 
																										"(I)Ljava/lang/Integer;");
				case IRBool =>
					mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", 
										"(Z)Ljava/lang/Boolean;");
				case _ =>
			}
			mv.visitInsn(AASTORE);
			temp = temp.tail
			i = i + 1
		}
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/reflect/Method", "invoke", 
									"(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;");
		mv.visitInsn(POP);
		mv.visitLabel(l1);
		val l6 = new Label();
		mv.visitJumpInsn(GOTO, l6);
		mv.visitLabel(l2);
		val mapSize = map.size
		mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, 
									Array[java.lang.Object]("java/lang/ClassNotFoundException"));
		mv.visitVarInsn(ASTORE, mapSize+1);
		mv.visitJumpInsn(GOTO, l6);
		mv.visitLabel(l3);
		mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, 
										Array[java.lang.Object]("java/lang/NoSuchMethodException"));
		mv.visitVarInsn(ASTORE, mapSize+1);
		mv.visitJumpInsn(GOTO, l6);
		mv.visitLabel(l4);
		mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, 
										Array[java.lang.Object]("java/lang/IllegalAccessException"));
		mv.visitVarInsn(ASTORE, mapSize+1);
		mv.visitJumpInsn(GOTO, l6);
		mv.visitLabel(l5);
		mv.visitFrame(Opcodes.F_SAME1, 0, null, 1,
				Array[java.lang.Object]("java/lang/reflect/InvocationTargetException"));
		mv.visitVarInsn(ASTORE, mapSize+1);
		mv.visitLabel(l6);
		mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
	}
	
	private def getBytecodeStore(tpe: IRType) = {
		tpe match {
			case IRInt => ISTORE
			case IRBool => ISTORE
			case IRStr => ASTORE
			case IRObject(x) => ASTORE
			case _ => throw new Error("Impossible type")
		}
	}
	
	
	private def getBytecodeStrConcat(clazz: IRClass, mv: MethodVisitor, 
									v1: IRVar, v2: IRVar, map: Map[IRVar, Int]) = {
		mv.visitTypeInsn(NEW, "java/lang/StringBuilder")
		mv.visitInsn(DUP)
		mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", 
																														"<init>", "()V")
		loadVar(clazz, mv, v2, map)
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", 
										"append", "("+ getBytecodeType(v2.tpe)
																+")Ljava/lang/StringBuilder;")
		loadVar(clazz, mv, v1, map)
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", 
																"append", "("+ getBytecodeType(v1.tpe)
																+")Ljava/lang/StringBuilder;")
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", 
																	"toString", "()Ljava/lang/String;")
		
	}
	private def getBytecodeOperation(op: IROp.Value, 
																tpe1: IRType, tpe2: IRType): Int = {
		op match {
			case IROp.Add =>
				(tpe1, tpe2) match {
					case (IRInt, IRInt) => IADD
					case _ => throw new Error("Impossible operation")
				}
			case IROp.Sub => ISUB
			case IROp.Mul => IMUL
			case IROp.Div => IDIV
			case IROp.Mod => IREM
			case _ => throw new Error("Impossible operation")
		}
	}
	private def getReturnBytecode(method: IRMethod) = {
		method.tpe match {
			case IRInt => IRETURN
			case IRBool => IRETURN
			case IRStr => ARETURN
			case IRObject(x) => ARETURN
			case _ => throw new Error("Not possible")
		}
	}
	private def getBytecodeMethodType(params: List[IRVar], rtrn: IRType) = {
		val pTypesList = for(param <- params) yield getBytecodeType(param.tpe)
		"("+ pTypesList.mkString + ")" +getBytecodeType(rtrn)
	}
	private def getBytecodeInitType(params: List[IRVar]) = {
		val pTypesList = for(param <- params) yield getBytecodeType(param.tpe)
		"("+ pTypesList.mkString + ")V"
	}
	private def getBytecodeType(tpe: IRType): String = {
		tpe match {
			case IRInt => "I"
			case IRBool => "Z"
			case IRStr => "Ljava/lang/String;"
			case IRVoid => "V"
			case IRObject(x) => "L" + x +";"
			case IRNoType => throw new Error("Impossible type")
		}
	}
	
	private def findVarType(exprs: List[IRExpr], name: String): IRType = {
			exprs match {
			case Nil => throw new Error("Not possible")
			case x :: xs if(x.isInstanceOf[IRVar]) => 
				if(x.asInstanceOf[IRVar].name == name) x.asInstanceOf[IRVar].tpe 
				else findVarType(xs, name)
			case x :: xs => findVarType(xs, name)
		}
	}
}