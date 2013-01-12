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

package ch.usi.inf.l3.moolc.ir

import _root_.ch.usi.inf.l3.moolc.ast._
import org.objectweb.asm.Label

class IntermediateCode(pgm: Program) {
	private var tcounter = 0
	private var lastVar: IRVar = null
	
	private def getLastVar = {
		if(lastVar == null) 
			throw new Error("Should not happen")
		lastVar
	}
	
	private def getNewTemp() = {
		val str = "temp_" + tcounter
		tcounter += 1
		str
	}
	
	def start(): IRProgram = {
		val classes = for(clazz <- pgm.classes) yield {
			val classVars = convertVarsToIRVar(clazz.body.vars)
			val classParams = convertVarsToIRVar(clazz.args)
			val methods = for(method <- clazz.body.methods) yield {
				val params = convertVarsToIRVar(method.args)
				val exprs = removeExtraVars(stmt(method.expr), params ::: classParams)
				val isStatic = method.mod match{
					case ClassMod => true
					case InstanceMod => false
				}
				IRMethod(method.name, getType(method.tpe), isStatic, 
														params, exprs)
			}
			val const = 
				removeExtraVars(stmt(clazz.body.const), classVars ::: classParams)
			IRClass(clazz.name.name, classVars, classParams, 
							const , methods)
		}
		
		IRProgram(classes)
	}
	
	private def removeExtraVars(es: List[IRExpr], vars: List[IRVar]): List[IRExpr] = {
		var exprs = es
		var temp: List[IRExpr] = Nil
		while(exprs != Nil){
			val expr = exprs.head
			exprs = exprs.tail
			expr match {
				case v: IRVar if(vars.contains(v) || exprs.contains(v)) =>
				case _ => temp = expr :: temp
			}
		}
		temp
	}
	private def getNewLabel() = {
		IRLabel(new Label())
	}
	
	
	private def convertVarsToIRVar(vars: List[Var]): List[IRVar] = {
		for(v <- vars) yield IRVar(v.name, getType(v.tpe))
	}
	private def stmt(expr: Expression): List[IRExpr] = stmtAux(expr, Nil)
	
	private def stmtAux(expr: Expression, list: List[IRExpr]): List[IRExpr] = {
		expr match{
			case Constant(v, _) => 
				val c = IRConst(getValue(v))
				val vr = IRVar(getNewTemp, c.getType)
				val assign = IRAssignConst(vr, c)
				lastVar = vr
				assign :: vr :: list
			case Var(x, tpe, _, _) => 
				val v = IRVar(x, getType(tpe))
				lastVar = v
				v :: list
			case Assign(v, e, _) => 
				val l1 = stmtAux(v, list)
				val vr = getLastVar
				val l2 = stmtAux(e, l1)
				l2.head match {
					case x: IRConst => IRAssignConst(vr, x) :: l2
					case x: IRVar => IRAssignVar(vr, x) :: l2
					case x: IRCall => IRAssignCall(vr, x) :: l2
					case x: IRNew => IRAssignNew(vr, x) :: l2
					case IRAssignConst(_, _) |
						 		IRAssignVar(_, _) |
								IRAssignCall(_, _) |
								IRAssignNew(_, _) |
								IRBinary(_, _, _, _)=> IRAssignVar(vr, getLastVar) :: l2
					case _ => throw new Error("Should not happen " + l2.head)
				}
			case Seq(e1, e2, _) => stmtAux(e2, stmtAux(e1, list))
			case Binary(ob, e1, e2, _) => 
				val op = convertToIROp(ob)
				val l1 = stmtAux(e1, list)
				val vr1 = getLastVar
				val l2 = stmtAux(e2, l1)
				val vr2 = getLastVar
				val vr3 = IRVar(getNewTemp(), findType(op, vr1, vr2))
				val binary = IRBinary(vr3, vr1, op, vr2)
				lastVar = vr3
				binary :: vr3 :: l2
			case Condition(c, t, f, _) =>
				val l1 = stmt(c)
				val v = IRNotVar(getLastVar)
				v.value.tpe = IRBool
				val lelse = getNewLabel
				val lnext = getNewLabel
				val cond = IRIF(v, lelse)
				val l2 = stmt(t)
				val l3 = stmt(f)
				val r = ((lnext :: l3) ::: (lelse :: IRGoto(lnext) :: l2)) ::: (cond :: l1)
				r ::: list
			case While(c, l, _) =>
				val ltest = getNewLabel
				val lbody = getNewLabel
				val l1 = stmt(c)
				val v = getLastVar
				val l2 = stmt(l)
				v.tpe = IRBool
				val cond = IRWhileIf(v, lbody)
				val r = ((cond :: l1) ::: List(ltest)) ::: l2 ::: List(lbody, IRGoto(ltest))
				r ::: list
			case Print(str, _) =>
				val l1 = stmt(str)
				val v = getLastVar
				v.tpe = IRStr
				(IRPrint(v) :: l1) ::: list
			case StaticCall(c, m, args, _) => 
				processCall(c.name, m, args, list)
			case DynamicCall(o, m, args, _) => 
				processCall((o.tpe.asInstanceOf[ClassName]).name, m, 
																										args, list, false, o.name)
			case This(c, m, args, _) =>
				processCall(c.name, m, args, list, false, "this")
			case Invoke(e, m, args, _) =>
				val l2 = stmtAux(e, list)
				val cc = getLastVar
				val l3 = stmtAux(m, l2)
				val mc = getLastVar
				var l1 = l3
				val irArgs = for(arg <- args) yield {
					l1 = stmtAux(arg, l1)
					getLastVar
				}
				irArgs.reverse
				IRInvoke(cc, mc, irArgs) :: l1
			case New(c, args, _) => 
				var l1 = list
				val irArgs = for(arg <- args) yield {
					l1 = stmtAux(arg, l1)
					getLastVar
				}
				irArgs.reverse
				val nw = IRNew(c.name, irArgs)
				val v = IRVar(getNewTemp, IRObject(c.name))
				val assign = IRAssignNew(v, nw)
				lastVar = v
				assign :: v :: l1
			case CT(e1, e2, _) =>
				stmtAux(e1, list)
			case RT(e, _) =>
				stmtAux(e, list)
			case IsCT(e, _) => 
				val v = IRVar(getNewTemp, IRBool)
				val assign = IRAssignConst(v, IRConst(IRBoolValue(true)))
				lastVar = v
				assign :: v :: list
			case Return(e, _) => 
				val l1 = stmt(e)
				val v = getLastVar
				(IRReturn(v) :: l1) ::: list
			case Empty | Semi | ClassName(_, _) | ObjectValue(_)=> list
		}
	}
	
	private def convertToIROp(op: Operation) = {
		op match{
			case Add => IROp.Add
			case Sub => IROp.Sub
			case Mul => IROp.Mul
			case Div => IROp.Div
			case Eq => IROp.Eq
			case Neq => IROp.Neq
			case Gt => IROp.Gt
			case Lt => IROp.Lt
			case Mod => IROp.Mod
			case _ => throw new Error("Not possible")
		}
	}
	private def findType(op: IROp.Value, e1: IRVar, e2: IRVar): IRType = {
		op match {
			case IROp.Add =>
				(e1.tpe, e2.tpe) match {
					case (IRVoid, _) 
							| (_, IRVoid)
							| (IRBool, _)
							| (_, IRBool)
							| (IRObject(_), _)
							| (_, IRObject(_)) => throw new Error("Not possible") 
					case (IRNoType, x) => x
					case (x, IRNoType) => x
					case (IRStr, IRStr) => IRStr
					case (IRStr, IRInt) => IRStr
					case (IRInt, IRStr) => IRStr
					case (IRInt, IRInt) => IRInt
				}
			case IROp.Sub => IRInt
			case IROp.Mul => IRInt
			case IROp.Div => IRInt
			case IROp.Eq => IRBool
			case IROp.Neq => IRBool
			case IROp.Gt => IRBool
			case IROp.Lt => IRBool
			case IROp.Mod => IRBool
			case _ => throw new Error("Not possible")
		}
	}
	private def getType(tpe: Types): IRType ={
		tpe match {
			case INT => IRInt
			case Bool => IRBool
			case Str => IRStr
			case Void => IRVoid
			case Unknown => IRNoType
			case ClassName(x, _) => IRObject(x)
			case _ => throw new Error("This should not happen")
		}
	}
	
	private def processCall(c: String, m: String, 
							args: List[Expression], list: List[IRExpr], 
								isStatic: Boolean = true,
								obj: String = "") : List[IRExpr] = {
		var l1 = list
		val irArgs = for(arg <- args) yield {
			l1 = stmtAux(arg, l1)
			getLastVar
		}
		irArgs.reverse
		var call = isStatic match {
			case true => IRCall(c, m, irArgs, isStatic)
			case false => IRCall(obj, m, irArgs, isStatic)
		}
		val tpe = getReturnType(ClassName(c, NoPosition), m)
		if(tpe != IRVoid){
			val v = IRVar(getNewTemp, tpe)
			val assign = IRAssignCall(v, call)
			lastVar = v
			assign :: v :: l1
		}
		else {
			call :: l1
		}
	}
	
	private def getReturnType(c: ClassName, m: String): IRType = {
		val clazz = pgm.getClass(c).get
		val method = clazz.getMethod(m).get
		getType(method.tpe)
	}
	
	private def getValue(v: Premitive) ={
		v match{
			case Null => IRNullValue
			case MString(s) => IRStringValue(s)
			case MInt(i) => IRIntValue(i)
			case MBool(b) => IRBoolValue(b)
		}
	}
}

sealed trait IRAssign {}
sealed trait IRExpr {}
case class IRConst(value: IRPremitive) extends IRExpr {
	def getType = value.getType
}
case class IRBinary(e1: IRVar, e2: IRVar, op: IROp.Value, e3: IRVar) extends IRExpr {}
case class IRAssignVar(e1: IRVar, v: IRVar) extends IRExpr with IRAssign{}
case class IRAssignConst(e1: IRVar, c: IRConst) extends IRExpr with IRAssign{}
case class IRAssignCall(e1: IRVar, f: IRCall) extends IRExpr with IRAssign{}
case class IRAssignNew(e1: IRVar, n: IRNew) extends IRExpr with IRAssign {}
case class IRVar(name: String, var tpe: IRType) extends IRExpr {
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[IRVar]) false
		else if (this.name == that.asInstanceOf[IRVar].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
	
}
case class IRNotVar(value: IRVar) extends IRExpr {}
case class IRLabel(l: Label) extends IRExpr {}
case class IRIF(c: IRNotVar, goto: IRLabel) extends IRExpr {}
case class IRWhileIf(c: IRVar, goto: IRLabel) extends IRExpr {}
case class IRGoto(l: IRLabel) extends IRExpr {}
case class IRPrint(v: IRVar) extends IRExpr {}
case class IRCall(c: String, m: String, args: List[IRVar], isStatic: Boolean) extends IRExpr {}
case class IRInvoke(c: IRVar, m: IRVar, args: List[IRVar]) extends IRExpr {}
case class IRNew(c: String, args: List[IRVar]) extends IRExpr {}
case class IRReturn(v: IRVar) extends IRExpr {}


sealed abstract class IRBlock {}
case class IRProgram(classes: List[IRClass]) extends IRBlock {
	def getClass(name: String): Option[IRClass] = {
		getClassAux(name, classes)
	}
	
	private def getClassAux(n: String, ns: List[IRClass]): Option[IRClass] = {
		ns match {
			case Nil => None
			case x :: xs => if(x.name == n) Some(x) else getClassAux(n, xs)
		}
	}
}
case class IRClass(name: String, vars: List[IRVar], args: List[IRVar], 
							const: List[IRExpr], methods: List[IRMethod]) extends IRBlock {
	def getMethod(name: String): Option[IRMethod] = {
		getMethodAux(name, methods)
	}
	
	private def getMethodAux(n: String, ns: List[IRMethod]): Option[IRMethod] = {
		ns match {
			case Nil => None
			case x :: xs => if(x.name == n) Some(x) else getMethodAux(n, xs)
		}
	}
	
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[IRClass]) false
		else if (this.name == that.asInstanceOf[IRClass].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}
case class IRMethod(name: String, tpe: IRType, isStatic: Boolean, 
							args: List[IRVar], body: List[IRExpr]) extends IRBlock {
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[IRMethod]) false
		else if (this.name == that.asInstanceOf[IRMethod].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}
	
	
	
object IROp extends Enumeration{
	type IROp = Value
	val Add, Sub, Mul, Div, Eq, Neq, Gt, Lt, Mod, Assign = Value
}

sealed trait IRPremitive {
	type A
	def get: String
	def getType: IRType
	def getValue: A
}
case class IRIntValue(value: Int) extends IRPremitive {
	type A = Int
	def get = value + ""
	def getType = IRInt
	def getValue = value
}
case class IRBoolValue(value: Boolean) extends IRPremitive {
	type A = Boolean
	def get = value + ""
	def getType = IRBool
	def getValue = value
}
case class IRStringValue(value: String) extends IRPremitive {
	type A = String
	def get = "\"" + value + "\""
	def getType = IRStr
	def getValue = value
}
case object IRNullValue extends IRPremitive {
	type A = Null
	def get = "null"
	def getType = IRNoType
	def getValue = null
}
	
	
sealed trait IRType {}
case object IRInt extends IRType{}
case object IRBool extends IRType{}
case object IRVoid extends IRType {}
case object IRStr extends IRType{}
case class IRObject(clazz: String) extends IRType{}
case object IRNoType extends IRType {}