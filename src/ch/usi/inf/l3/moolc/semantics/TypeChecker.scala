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

package ch.usi.inf.l3.moolc.semantics

import _root_.ch.usi.inf.l3.moolc.parser._
import _root_.ch.usi.inf.l3.moolc.ast._
import _root_.ch.usi.inf.l3.moolc.lexer._

class TypeChecker(expr: Expression, scope: Scope, 
															lines: Array[String], pgm: Program,
															tpe: Types = Unknown) {
	
	def start() = typeCheck(expr)
	
	private def typeCheck(exp: Expression): Types = {
		exp match {
			case v @ Var(_, Unknown, _, _) => scope.getType(v)
			case v @ Var(_, t, _, _) => t
			case Assign(e1, e2, pos) =>
				val t1 = typeCheck(e1)
				check(t1, typeCheck(e2), pos)
				t1
			case Seq(e1, e2, _) =>
				typeCheck(e1)
				typeCheck(e2)
			case Return(e, pos) => 
				val tp = typeCheck(e)
				check(tpe, tp, pos)
				tpe
			case Binary(op, e1, e2, pos) =>
				val t1 = typeCheck(e1)
				val t2 = typeCheck(e2)
				operability(op, t1, t2, pos)
				resultType(op, t1, t2, pos)
			case While(e1, e2, pos) =>
				check(typeCheck(e1), Bool, pos)
				typeCheck(e2)
			case CT(_, e2, pos) => 
				val t1 = typeCheck(e2)
				check(t1, Bool, pos)
				Bool
			case Condition(c, t, f, pos) =>
				check(typeCheck(c), Bool, pos)
				typeCheck(t)
				typeCheck(f)
			case Print(expr, pos) => 
				check(typeCheck(expr), Str, pos)
				Void
			case IsCT(e, pos) => Bool
			case This(c, m, args, pos) =>
				val clazz = pgm.getClass(c).get
				clazz.getMethod(m) match {
					case Some(method) => 
						if(getArgsTypes(args) != getParamsTypes(method.args))
							error("Invalid argument type", pos)
						method.tpe
					case _ => error("Invalid method name " + m, pos)
				}
			case DynamicCall(o, m, args, pos) =>
				val to = scope.getType(o)
				to match {
					case c: ClassName =>
						pgm.getClass(c) match{
							case Some(clazz) => 
								clazz.getMethod(m) match {
									case Some(method) => 
										if(getArgsTypes(args) != getParamsTypes(method.args))
											error("Invalid argument type", pos)
										method.tpe
									case _ => error("Invalid method name " + m, pos)
								}
							case None => error("Invalid class for object, " + o, pos)
						}
					case _ => error("Invalid method name for object, " + o, pos)
				}		
			case StaticCall(c, m, args, pos) =>
				val cName = typeCheck(c)
				cName match{
					case c: ClassName =>
						pgm.getClass(c) match {
							case Some(clazz) =>
								clazz.getMethod(m) match{
									case Some(method) =>
										if(method.mod != ClassMod) 
											error("Method " + m + " is an instance method", pos)
										if(getArgsTypes(args) != getParamsTypes(method.args))
											error("Invalid argument type", pos)
										method.tpe
									case _ => error("Invalid method name " + m, pos)
								}
							case None => error("Invalid class name, " + c, pos)
						}
					case _ => error("Invalid class name, " + c, pos)
				}
			case Invoke(c, m, args, pos) =>
				typeCheck(c) match {
					case ClassName(_, _) | Str => 
					case _ => error("Invalid argument to method Invoke, " + c, pos)
				}
				check(typeCheck(m), Str, pos)
				Void
			case New(c, args, pos) =>
				val clazz = pgm.getClass(c)
				clazz match{
					case None => error("Class " + c + " is not defined", pos)
					case Some(cc) => 
						if(getArgsTypes(args) != getParamsTypes(cc.args))
							error("Invalid argument type", pos)
				}
				c
			case _: RT => Void
			case x: ClassName => x
			case x: Constant => getConstantType(x)
			case Empty => Void
			case Semi => Void
		}
	}
	
	private def check(t1: Types, t2: Types, pos: Pos) = {
		(t1, t2) match {
			case (INT, INT) |
					(Bool, Bool) |
					(Str, Str) |
					(ClassName(_, _), NullType) => true
			case (x: ClassName, y: ClassName) => (x.name == y.name)
			case _ => error("Incompatible types", pos)
		}
	}
	
	private def operability(op: Operation, t1: Types, t2: Types, pos: Pos) = {
		(t1, t2) match {
			case (INT, INT) |
					(Bool, Bool) |
					(Str, Str) => true
			case (Str, INT) if(op == Add) => true
			case (INT, Str) if(op == Add)=> true
			case _ => error("Incompatible types", pos)
		}
	}
	
	private def resultType(op: Operation, t1: Types, t2: Types, pos: Pos) = {
		op match {
			case Add =>
				(t1, t2) match {
					case (Str, Str) => Str
					case (INT, INT) => INT
					case (INT, Str) => Str
					case (Str, INT) => Str
					case (_, _)=> error(op + "Cannot appear here", pos)
				}
			case Sub if(t1 == INT && t2 == INT) => INT
			case Mul if(t1 == INT && t2 == INT) => INT
			case Div if(t1 == INT && t2 == INT) => INT
			case Eq if(t1 == t2) => Bool
			case Neq if(t1 == t2)=> Bool
			case Gt if(t1 == INT && t2 == INT) => Bool
			case Lt if(t1 == INT && t2 == INT) => Bool
			case Mod if(t1 == INT && t2 == INT) => INT
			case _ => error(op + " Cannot appear here", pos)
		}
	}
	
	private def getArgsTypes(args: List[Expression]) = {
		getArgsTypesAux(args, Nil)
	}
	
	private def getArgsTypesAux(args: List[Expression], 
													types: List[Types]): List[Types] = {
		args match {
			case Nil => types
			case x :: xs => getArgsTypesAux(xs, typeCheck(x) :: types)
		}
	}
	private def getParamsTypes(vars: List[Var]) = {
		getParamsTypesAux(vars, Nil)
	}
	
	private def getParamsTypesAux(vars: List[Var], 
									types: List[Types]): List[Types] = {
		vars match {
			case Nil => types
			case x :: xs => getParamsTypesAux(xs, x.tpe :: types)
		}
	}
	
	private def getConstantType(c: Constant): Types = {
		c.value match {
			case _: MString => Str
			case _: MInt => INT
			case _: MBool => Bool
			case Null => NullType
			case _ => error("Type is not recognized", NoPosition)
		}
	}
	
	private def error(str: String, pos: Pos) = {
		throw new Error(pos match {
			case Position(x, y) => str + ", in line "+ (y+1) +"\n" +
				 lines(y) + "\n" + (" " * (x+13)) + "^"
			case _ => str
		})
	}
}