package ch.usi.inf.l3.moolc.evaluator

import com.microsoft.z3._
import scala.language.implicitConversions

class SMTFrontend{
  
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
  
  def eval(a: Expr) = solver.Model.Evaluate(a, false)
  
  
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
  def getValue(b: Expr, pc: BoolExpr) = {
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
  
  def dispose() = context.Dispose()
}

object SMTFrontend{
  val UNSATISFIABLE = Status.UNSATISFIABLE
  val UNKNOWN = Status.UNKNOWN
  val SATISFIABLE = Status.SATISFIABLE
}
