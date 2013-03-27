/*
 * Mool Compiler, is a toy compiler written in Scala, which compiles programs
 * written in Mool language to JVM bytecode
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


package ch.usi.inf.l3.moolc.evaluator

import com.microsoft.z3._
import scala.language.implicitConversions

/**
 * A simple class, aims to make working with Java Bindings for Z3 a bit more
 * Scala-ish!
 */

class SMTFrontend{
  
  /**
   * Import whatever static is defined in <code>SMTFrontend</code> object!
   */
  
  import SMTFrontend._
  
  private val context = new Context
  private var solver = context.MkSolver
  
  def const(i: Int) = context.MkInt(i)
  def const(b: Boolean) = context.MkBool(b)
  
  implicit def int2MkInt(i: Int) = const(i)
  
  implicit def bool2MkBool(b: Boolean) = const(b)
  
  def int(name: String) = context.MkIntConst(name)
  
  def bool(name: String) = context.MkBoolConst(name)
  
  def equ(e1: Expr, e2: Expr) = context.MkEq(e1, e2)
  
  def not(e: BoolExpr) = context.MkNot(e)
  
  def and(exprs: BoolExpr*) = context.MkAnd(exprs.toArray)
  
  def or(exprs: BoolExpr*) = context.MkOr(exprs.toArray)
  
  def and(exprs: Array[BoolExpr]) = context.MkAnd(exprs)
  
  def or(exprs: Array[BoolExpr]) = context.MkOr(exprs)
  
  def execute(e: BoolExpr) = solver.Assert(e)
  
  def execute(e: Array[BoolExpr]) = solver.Assert(e)
  
  def check() = solver.Check
  
  def eval(a: Expr) = {
    val result = solver.Model.Evaluate(a, false)
    try { 
      result.toString.toInt
    } catch {
      case e: NumberFormatException => -1
    }
  }
  
  def enterScope() = solver.Push
  
  def leaveScope() = solver.Pop
  
  /*
   * Tests weather the path is satisfiable or not. There should be one
   * and only one possible solution, otherwise -1 is returned.
   *
   * If it is then the index of the value in will be returned, -1
   * otherwise
   *
   * @param b is the variable that needs to be looked up
   * @param pc is the path condition
   */
  def getValue(b: Expr, pc: BoolExpr): Int = {
    // execute the path condition
    execute(pc)
    // is it satisfiable or not
    if(check != SATISFIABLE){
      -1
    }else{
      // If there is a solution, make sure that this is the only solution
      val valB = eval(b)
      
      // This is a hack, it simply adds another condition to the path condition
      // saying: suppose that b != valB, can you find another solution?
      // if the answer is yes, then there are more than one solution and we
      // return -1, otherwise we return valBin
      enterScope
      val temp = not(equ(b, valB))
      execute(and(pc, temp))
      if(check != SATISFIABLE){
        leaveScope
        valB
      }else{
        leaveScope
        -1
      }
    }
  }
  
  /**
   * This methods solves the Constraints (several times) for this object, 
   * and returns all possible solutions.
   * 
   * @param b is the variable that needs to be looked for
   * @param pc is the path condition
   * @return a list of all possible solutions
   */
  def getValues(b: Expr, pc: BoolExpr): List[Int] = {
    def getVal(pc: BoolExpr): Int = {
      execute(pc)
      if(check != SATISFIABLE){
        -1
      }else{
        eval(b)
      }
    }
    
    def getVals(pc: BoolExpr, list: List[Int]): List[Int] = {
      getVal(pc) match{
        case -1 => list
        case x => 
          enterScope
          val expr = not(equ(b, x))
          val result = getVals(expr, x :: list)
          leaveScope
          result
      }
    }
    getVals(pc, Nil)
    
  }
  
  def dispose() = context.Dispose()
}

object SMTFrontend{
  val UNSATISFIABLE = Status.UNSATISFIABLE
  val UNKNOWN = Status.UNKNOWN
  val SATISFIABLE = Status.SATISFIABLE
}
