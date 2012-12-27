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


here is a change
package ch.usi.inf.l3.moolc.semantics

import _root_.ch.usi.inf.l3.moolc.parser._
import _root_.ch.usi.inf.l3.moolc.ast._
import _root_.ch.usi.inf.l3.moolc.lexer._

class SemanticsAnalyzer(pgm: Program, lines: Array[String]) {
	
	def start() = symboleTableCheck
	private def symboleTableCheck() = {
		checkDuplicateClass
		for(clazz <- pgm.classes) typeCheckClass(clazz)
	}
	
	private def typeCheckClass(clazz: MClass) = {
		val scope = new ClassScope
		val impl = clazz.body
		for(v <- impl.vars) scope.addVar(v)
		typeCheckInitBody(clazz, impl.const, scope.getNestedScope)
		val methods = impl.methods
		for(method <- methods) {
			scope.addMethod(method)
			val mscope = scope.getNestedScope
			for(param <- method.args) mscope.addVar(param)
			typeCheckMethodBody(method, mscope)
		}
		for(param <- clazz.args) scope.addVar(param)
	}
	
	
	private def typeCheckInitBody(clazz: MClass, expr: Expression, scope: Scope) = {
		for(param <- clazz.args) scope.addVar(param)
		val iscope = typeCheckExpr(expr, scope)
	}
	
	private def typeCheckMethodBody(m: MMethod, scope: Scope) = {
		val mscope = typeCheckExpr(m.expr, scope)
	}
	
	// this method does not check against method calls and field selections:
	// StaticCall, DynamicCall, Invoke, New should be checked separately too
	private def typeCheckExpr(expr: Expression, scope: Scope): Scope = {
		expr match {
			case v @ Var(_, Unknown, _) => 
				scope.getType(v)
				scope
			case v @ Var(_, _, _) => 
				scope.addVar(v)
				scope
			case Assign(e1, e2, _) =>
				val t = typeCheckExpr(e1, scope)
				typeCheckExpr(e2, t)
			case Seq(e1, e2, _) =>
				val t = typeCheckExpr(e1, scope)
				typeCheckExpr(e2, t)
			case Binary(_, e1, e2, _) =>
				val t = typeCheckExpr(e1, scope)
				typeCheckExpr(e2, t)
			case While(e1, e2, _) =>
				val t = typeCheckExpr(e1, scope)
				typeCheckExpr(e2, t)
			case CT(e1, e2, _)=>
				val t = typeCheckExpr(e1, scope)
				typeCheckExpr(e2, t)
			case Condition(c, t, f, _) =>
				val t1 = typeCheckExpr(c, scope)
				typeCheckExpr(t, t1)
				typeCheckExpr(t, t1)
			case StaticCall(_, _, args, _) => 
				for(arg <- args) typeCheckExpr(arg, scope)
				scope
			case DynamicCall(v, _, args, _) => 
				typeCheckExpr(v, scope)
				for(arg <- args) typeCheckExpr(arg, scope)
				scope
			case Invoke(e1, e2, args, _) =>
				typeCheckExpr(e1, scope)
				typeCheckExpr(e2, scope)
				for(arg <- args) typeCheckExpr(arg, scope)
				scope
			case New(_, args, _) =>
				for(arg <- args) typeCheckExpr(arg, scope)
				scope
			case RT(e, _) => typeCheckExpr(e, scope)
			case IsCT(e, _) => typeCheckExpr(e, scope)
			case c @ ClassName(n, ps) => {
				pgm.classes.contains(MClass(c, Nil, null, NoPosition, Map.empty)) match{
					case true => scope
					case false => error("Class " + n + " is not defined", ps)
				}
			}
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
			case Position(x, y) => str + ", in line "+ (x+1) +"\n" +
				 lines(x) + "\n" + (" " * y) + "^"
			case _ => str
		})
	}
	
	abstract class Scope {
		protected var envVar: Map[String, Types]
		def getEnvVar = envVar
		def addVar(v: Var): Unit = {
			if(v.tpe == Unknown) 
				error("Variable " + v.name +" has no type", v.pos)
			val t = envVar.get(v.name) 
			t match {
				case Some(x) => error("Variable " + v.name + " is already initialized", v.pos)
				case None => envVar = envVar + (v.name -> v.tpe)
			}
		}
	
		def getType(v: Var): Types = {
			val tpe = envVar.get(v.name) 
			tpe match {
				case Some(x) => x
				case None => error("Variable " + v.name + " is not initialized", v.pos)
			}
		}
	
		def getNestedScope: Scope = {
			new ScopeImpl(envVar)
		}
		
		final private class ScopeImpl(envV: Map[String, 
						Types] = Map.empty) extends Scope {
			protected var envVar = envV
		}
	}
	
	final class ClassScope(envV: Map[String, Types] = Map.empty,
			envM: Map[String, MMethod] = Map.empty) extends Scope{
		protected var envVar = envV
		var envMeth = envM
		
		def getEnvMeth = envMeth
		
		def addMethod(m: MMethod) = {
			val mthd = envMeth.get(m.name) 
			mthd match{
				case Some(x) => error("Method " + m.name + " is already defined", m.pos)
				case None => envMeth = envMeth + (m.name -> m)
			}
		}
		
		def getMethodType(m: MMethod): Types = {
			val mthd = envMeth.get(m.name) 
			mthd match {
				case Some(x) => x.tpe
				case None => error("Method " + m.name + "is not defined", m.pos)
			}
		}
	}
}
