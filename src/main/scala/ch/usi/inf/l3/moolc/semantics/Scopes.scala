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

import _root_.ch.usi.inf.l3.moolc.ast._

abstract class Scope {
	protected var envVar: Map[String, Types]
	protected var lines: Array[String]
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
	
	def hasVar(v: Var): Boolean = {
		envVar.contains(v.name)
	}
	
	def getType(v: Var): Types = {
		val tpe = envVar.get(v.name) 
		tpe match {
			case Some(x) => x
			case None => error("Variable " + v.name + " is not initialized", v.pos)
		}
	}
	
	def getNestedScope: Scope = {
		new ScopeImpl(envVar, lines)
	}
		
	final private class ScopeImpl(envV: Map[String, 
					Types] = Map.empty, ls: Array[String]) extends Scope {
		protected var envVar = envV
		protected var lines = ls
	}
	
	protected def error(str: String, pos: Pos) = {
		throw new Error(pos match {
			case Position(x, y) => str + ", in line "+ (y+1) +"\n" +
				 lines(y) + "\n" + (" " * (x+13)) + "^"
			case _ => str
		})
	}
}
	
final class ClassScope(ls: Array[String], envV: Map[String, Types] = Map.empty,
		envM: Map[String, MMethod] = Map.empty) extends Scope{
	protected var envVar = envV
	protected var lines = ls
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
