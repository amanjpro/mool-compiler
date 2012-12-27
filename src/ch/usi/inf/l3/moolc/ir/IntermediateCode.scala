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

class IntermediateCode(pgm: Program) {
	private var tcounter = 0
	private var lcounter = 0
	private var lastVar: IRVar = null
	
	private def getLastVar = {
		if(lastVar == null) 
			throw new Error("Should not happen")
		lastVar
	}
	
	private def getNewTemp() = {
		val str = "t" + tcounter
		tcounter += 1
		str
	}
	
	def start(): IRProgram = {
		val classes = for(clazz <- pgm.classes) yield {
			val methods = for(method <- clazz.body.methods) yield {
				val exprs = stmt(method.expr).reverse
				val isStatic = method.mod match{
					case ClassMod => true
					case InstanceMod => false
				}
				IRMethod(method.name, getType(method.tpe), isStatic, 
														convertParams(method.args), exprs)
			}
			IRClass(clazz.name.name, convertParams(clazz.args), 
							stmt(clazz.body.const), methods)
		}
		
		IRProgram(classes)
	}
	
	private def getNewLabel() = {
		val str = "l" + lcounter
		lcounter += 1
		IRLabel(str)
	}
	
	private def convertParams(vars: List[Var]): List[IRVar] = {
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
				if(!list.contains(v))
					v :: list
				else list
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
					case _ => throw new Error("Should not happen" + l2.head)
				}
			case Seq(e1, e2, _) => stmtAux(e2, stmtAux(e1, list))
			case Binary(ob, e1, e2, _) => 
				val op = convertToIROp(ob)
				val l1 = stmtAux(e1, list)
				val vr1 = getLastVar
				val l2 = stmtAux(e2, l1)
				val vr2 = getLastVar
				val vr3 = IRVar(getNewTemp(), findType(op, vr1, vr2))
				val binary = IRBinary(vr3, vr2, op, vr1)
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
				val cond = WhileIf(v, lbody)
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
				processCall((o.tpe.asInstanceOf[ClassName]).name, m, args, list, false)
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
				IRInvoke(cc, mc, irArgs) :: list
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
				assign :: v :: list
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
			case Empty | Semi | ClassName(_, _) => list
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
								isStatic: Boolean = true) : List[IRExpr] = {
		var l1 = list
		val irArgs = for(arg <- args) yield {
			l1 = stmtAux(arg, l1)
			getLastVar
		}
		irArgs.reverse
		val v = IRVar(getNewTemp, getReturnType(ClassName(c, NoPosition), m))
		var call = IRCall(c, m, irArgs, isStatic)
		val assign = IRAssignCall(v, call)
		lastVar = v
		assign :: v :: list
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
case class IRVar(name: String, var tpe: IRType) extends IRExpr {}
case class IRNotVar(value: IRVar) extends IRExpr {}
case class IRLabel(l: String) extends IRExpr {}
case class IRIF(c: IRNotVar, goto: IRLabel) extends IRExpr {}
case class WhileIf(c: IRVar, goto: IRLabel) extends IRExpr {}
case class IRGoto(l: IRLabel) extends IRExpr {}
case class IRPrint(v: IRVar) extends IRExpr {}
case class IRCall(c: String, m: String, args: List[IRVar], isStatic: Boolean) extends IRExpr {}
case class IRInvoke(c: IRVar, m: IRVar, args: List[IRVar]) extends IRExpr {}
case class IRNew(c: String, args: List[IRVar]) extends IRExpr {}
case class IRCT(e: IRVar, ct: IRVar) extends IRExpr {}
case class IRRT(e: IRVar) extends IRExpr {}
case class IRIsCT(e: IRVar) extends IRExpr {}
case class IRReturn(v: IRVar) extends IRExpr {}


sealed abstract class IRBlock {}
case class IRProgram(classes: List[IRClass]) extends IRBlock {}
case class IRClass(name: String, args: List[IRVar], 
							const: List[IRExpr], methods: List[IRMethod]) extends IRBlock {}
case class IRMethod(name: String, tpe: IRType, isStatic: Boolean, 
							args: List[IRVar], body: List[IRExpr]) extends IRBlock {}
	
	
	
object IROp extends Enumeration{
	type IROp = Value
	val Add, Sub, Mul, Div, Eq, Neq, Gt, Lt, Mod, Assign = Value
}

sealed trait IRPremitive {
	def get: String
	def getType: IRType
}
case class IRIntValue(value: Int) extends IRPremitive {
	def get = value + ""
	def getType = IRInt
}
case class IRBoolValue(value: Boolean) extends IRPremitive {
	def get = value + ""
	def getType = IRBool
}
case class IRStringValue(value: String) extends IRPremitive {
	def get = "\"" + value + "\""
	def getType = IRStr
}
case object IRNullValue extends IRPremitive {
	type A = Nothing
	def get = "null"
	def getType = IRNoType
}
	
	
sealed trait IRType {}
case object IRInt extends IRType{}
case object IRBool extends IRType{}
case object IRVoid extends IRType {}
case object IRStr extends IRType{}
case class IRObject(clazz: String) extends IRType{}
case object IRNoType extends IRType {}