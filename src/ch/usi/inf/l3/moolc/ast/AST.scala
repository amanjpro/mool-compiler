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

package ch.usi.inf.l3.moolc.ast

sealed abstract class Pos {}
case class Position(col: Int, line: Int) extends Pos{
	require(col >= 0)
	require(line >= 0)
}
case object NoPosition extends Pos {}

sealed trait Premitive {
	type A
	def optionValue: Option[A]
}

sealed trait Value

case object Null extends Premitive with Value{
	type A = Nothing
	override def optionValue = None
}

case class MString(value: String) extends Premitive with Value{
	type A = String
	def optionValue = Some(value)
}

case class MInt(value: Int) extends Premitive with Value{
	type A = Int
	def optionValue = Some(value)
}

case class MBool(value: Boolean) extends Premitive with Value{
	type A = Boolean
	def optionValue = Some(value)
}
	

case class MObject(clazz: MClass, var env: Map[Var, Value]) extends Value {}



sealed abstract class Block {
	var env: Map[Var, Value]
}

case class Program(classes: List[MClass], var env: Map[Var, Value]) extends Block {
	def getClass(name: ClassName): Option[MClass] = {
		getClassAux(name, classes)
	}
	
	private def getClassAux(n: ClassName, ns: List[MClass]): Option[MClass] = {
		ns match {
			case Nil => None
			case x :: xs => if(x.name == n) Some(x) else getClassAux(n, xs)
		}
	}
}

case class MClass (name: ClassName, args: List[Var], body: MClassBody, pos: Pos, var env: Map[Var, Value]) extends Block {
	
	def getMethod(name: String): Option[MMethod] = {
		getMethodAux(name, body.methods)
	}
	
	private def getMethodAux(name: String, methods: List[MMethod]): Option[MMethod] = {
		methods match {
			case Nil => None
			case x :: xs => if(x.name == name) Some(x) else getMethodAux(name, xs)
		}
	}
	
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[MClass]) false
		else if (this.name == that.asInstanceOf[MClass].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}

case class MMethod(mod: Modifier, name: String, tpe: Types, 
		args: List[Var], pos: Pos, expr: Expression, 
			var env: Map[Var, Value]) extends Block{
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[MMethod]) false
		else if (this.name == that.asInstanceOf[MMethod].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}

case class MClassBody(val vars: List[Var], val const: Expression, 
																	val methods: List[MMethod]) {}

sealed abstract class Modifier {}

case object ClassMod extends Modifier {}

case object InstanceMod extends Modifier {}

sealed abstract class Operation {}

case object Add extends Operation {}
case object Sub extends Operation {}
case object Mul extends Operation {}
case object Div extends Operation {}
case object Eq extends Operation {}
case object Neq extends Operation {}
case object Lt extends Operation {}
case object Gt extends Operation {}
case object Mod extends Operation {}

sealed trait Types {}

case object INT extends Types {}
case object Str extends Types {}
case object Bool extends Types {}
case object Void extends Types {}	
case object NullType extends Types {}
case object Unknown extends Types {}

sealed abstract class Expression {}

case class Constant(value: Premitive, pos: Pos) extends Expression {
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[Constant]) false
		else if (this.value == that.asInstanceOf[Constant].value) true
		else false
	}
	override def hashCode(): Int = value.hashCode
}

case class Var(name: String, var tpe: Types, pos: Pos, 
								init: Boolean = false) extends Expression {
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[Var]) false
		else if (this.name == that.asInstanceOf[Var].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}

case class ClassName(name: String, pos: Pos) extends Expression with Types {
	override def equals(that: Any): Boolean = {
		if(that == null || !that.isInstanceOf[ClassName]) false
		else if (this.name == that.asInstanceOf[ClassName].name) true
		else false
	}
	override def hashCode(): Int = name.hashCode
}
case class Assign(vr: Var, value: Expression, pos: Pos) extends Expression {}

case class Seq(expr1: Expression, expr2: Expression, pos: Pos) extends Expression {}
case class Binary(op: Operation, expr1: Expression, expr2: Expression, pos: Pos) extends Expression {}
case class Condition(cond: Expression, tbranch: Expression, fbranch: Expression, pos: Pos) extends Expression {}
case class While(cond: Expression, body: Expression, pos: Pos) extends Expression {}
case class StaticCall(clazz: ClassName, method: String, args: List[Expression], pos: Pos) extends Expression {}
case class DynamicCall(obj: Var, method: String, args: List[Expression], pos: Pos) extends Expression {}
case class This(clazz: ClassName, method: String, args: List[Expression], pos: Pos) extends Expression {}
case class Invoke(cName: Expression, mName: Expression, args: List[Expression], pos: Pos) extends Expression {}
case class New(cName: ClassName, args: List[Expression], pos: Pos) extends Expression {}
case class CT(expr1: Expression, expr2: Expression, pos: Pos) extends Expression {}
case class RT(expr: Expression, pos: Pos) extends Expression {}
case class IsCT(expr: Expression, pos: Pos) extends Expression {}
case class Return(expr: Expression, pos: Pos) extends Expression {}
case class Print(expr: Expression, pos: Pos) extends Expression {}

case object Empty extends Expression {}
case object Semi extends Expression {}
 