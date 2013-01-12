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

class SymbolTableChecker(pgm: Program, lines: Array[String]) {
	
	def start() = symboleTableCheck
	private var currentClassScope: Scope = null
	private var isStaticMethod = false
	private def symboleTableCheck() = {
		checkDuplicateClass
		for(clazz <- pgm.classes) symbolTableCheckClass(clazz)
	}
	
	private def symbolTableCheckClass(clazz: MClass) = {
		val scope = new ClassScope(lines)
		val impl = clazz.body
		for(v <- impl.vars) scope.addVar(v)
		currentClassScope = scope
		isStaticMethod = false
		symbolTableCheckInit(clazz, impl.const, scope.getNestedScope)
		val methods = impl.methods
		for(method <- methods) {
			isStaticMethod = method.mod == ClassMod
			scope.addMethod(method)
			val mscope = scope.getNestedScope
			for(param <- method.args) mscope.addVar(param)
			symbolTableCheckMethod(method, mscope)
		}
		for(param <- clazz.args) scope.addVar(param)
	}
	
	
	private def symbolTableCheckInit(clazz: MClass, expr: Expression, scope: Scope) = {
		for(param <- clazz.args) scope.addVar(param)
		val iscope = symbolTableCheckExpr(expr, scope)
		new TypeChecker(expr, iscope, lines, pgm).start
	}
	
	private def symbolTableCheckMethod(m: MMethod, scope: Scope) = {
		val mscope = symbolTableCheckExpr(m.expr, scope)
		new TypeChecker(m.expr, mscope, lines, pgm, m.tpe).start
	}
	
	// this method does not check against method calls and field selections:
	// StaticCall, DynamicCall, Invoke, This and New should be 
	// checked separately too
	private def symbolTableCheckExpr(expr: Expression, scope: Scope): Scope = {
		expr match {
			case v1 @ Var(_, _, _, false) => 
				if(isStaticMethod && currentClassScope.hasVar(v1)) {
					error("Variable " + v1.name + " is static cannot be accessed within " +
						 			" an instance method", v1.pos)
				}
				v1.tpe = scope.getType(v1)
				scope
			case v2 @ Var(_, _, _, true) => 
				scope.addVar(v2)
				scope
			case Assign(e1, e2, _) =>
				val t = symbolTableCheckExpr(e1, scope)
				symbolTableCheckExpr(e2, t)
			case Seq(e1, e2, _) =>
				val t = symbolTableCheckExpr(e1, scope)
				symbolTableCheckExpr(e2, t)
			case Binary(_, e1, e2, _) =>
				val t = symbolTableCheckExpr(e1, scope)
				symbolTableCheckExpr(e2, t)
			case While(e1, e2, _) =>
				val t1 = symbolTableCheckExpr(e1, scope)
				symbolTableCheckExpr(e2, t1)
			case CT(e1, e2, _)=>
				val t = symbolTableCheckExpr(e1, scope.getNestedScope)
				symbolTableCheckExpr(e2, t)
			case Condition(c, t, f, _) =>
				val t1 = symbolTableCheckExpr(c, scope)
				val t2 = symbolTableCheckExpr(t, t1)
				symbolTableCheckExpr(f, t2)
			case Print(e, _) => 
				symbolTableCheckExpr(e, scope)
				scope
			case StaticCall(_, _, args, _) => 
				for(arg <- args) symbolTableCheckExpr(arg, scope)
				scope
			case DynamicCall(v, _, args, _) => 
				symbolTableCheckExpr(v, scope)
				for(arg <- args) symbolTableCheckExpr(arg, scope)
				scope
			case This(_, _, args, pos) =>
				if(isStaticMethod) {
					error("Cannot access `this` in a static context", pos)
				}
				for(arg <- args) symbolTableCheckExpr(arg, scope)
				scope
			case Invoke(e1, e2, args, _) =>
				symbolTableCheckExpr(e1, scope)
				symbolTableCheckExpr(e2, scope)
				for(arg <- args) symbolTableCheckExpr(arg, scope)
				scope
			case New(_, args, _) =>
				for(arg <- args) symbolTableCheckExpr(arg, scope)
				scope
			case RT(e, _) => symbolTableCheckExpr(e, scope)
			case IsCT(e, _) => symbolTableCheckExpr(e, scope)
			case c @ ClassName(n, ps) => {
				pgm.getClass(c) match{
					case Some(x) => scope
					case None => error("Class " + n + " is not defined", ps)
				}
			}
			case Return(e, _) => symbolTableCheckExpr(e, scope)
			case Constant(_, _)
					| Empty
					| Semi => scope
		}
	}
	
	private def checkDuplicateClass = {
		var classes = pgm.classes
		var test = false
		while(classes != Nil && !test) {
			val clazz = classes.head
			classes = classes.tail
			test = classes.contains(clazz)
			if(test) {
				error("Class " + clazz.name.name + " is already defined", clazz.pos)
			}
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