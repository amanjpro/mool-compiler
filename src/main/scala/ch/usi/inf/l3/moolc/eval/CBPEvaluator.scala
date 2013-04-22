/*
 * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

 

package ch.usi.inf.l3.moolc.evaluator

import _root_.ch.usi.inf.l3.moolc.ast._
 
 
class CBPEvaluator(pgm: Program) {
	private var staticMethodID = 0
	private var doCheckStore = true
  
	private val classBank = new ClassBank
	private var methodBankMap: Map[String, MethodBank] = Map.empty
  
 	def start = {
		val clazzes = for(clazz <- pgm.classes) yield {
			var store = addArgsToStore(clazz.body.vars)
			val cnstStore = addArgsToStore(clazz.args, store)
      
			val (const, _, constStore) = peval(clazz.body.const, cnstStore)
      store = store.addEnvToSelf(clazz.body.vars, constStore)
			
			
			val methods = for(method <- clazz.body.methods) yield {
				val mstore = addArgsToStore(method.args, store)
				val mbody = peval(method.expr, mstore)._1
				MMethod(method.mod, method.name, method.tpe, method.args,
							method.pos, mbody)
			}
			val smethods = methodBankMap.get(clazz.name.name) match{
				case Some(bank) => bank.getSpecializedMethodsList
				case _ => Nil
			}
			MClass(clazz.name, clazz.args, 
				MClassBody(clazz.body.vars, const, smethods ++ methods), clazz.pos)
		}
		Program(clazzes ++ classBank.getAllSpecializedClasses)
	}
  
  
	private def feval(expr: Expression, store: ConstraintStore) :
			(Expression, ConstraintStore) = {
		expr match{
			case x : Constant => (x, store)
			case obj: ObjectValue => (obj, store)
			case cname: ClassName => (cname, store)
			case Binary(op, x, y, _) => 
				val (x1, store1) = feval(x, store)
				val (y1, store2) = feval(y, store1)
				(x1, y1) match{
					case (Constant(a, _), Constant(b, _)) => (doOp(op, a, b), store2)
					case _ => (Binary(op, x1, y1, NoPosition), store2)
				}
			case x: Var => 
				store.get(x) match{
					case Some(v: PEKnownValue) => (v.getExpr, store)
					case Some(Top) => throw new Error("You cannot put CT here")
					case _ => (x, store)
				}
			case Assign(lhs, rhs, _) =>
				val (r, store1) = feval(rhs, store)
				(r, store1.add(lhs, CTValue(r)))
			case Seq(e1, e2, _) =>
				val (_, store1) = feval(e1, store)
				feval(e2, store1)
			case cond @ Condition(c, t, f, _) =>
				val (cond, condStore) = feval(c, store) 
        cond match {
					case (Constant(MBool(true), _)) => feval(t, condStore)
					case (Constant(MBool(false), _)) => feval(f, condStore)
					case _ => throw new Error(c + " You should not reach here")
				}
			case While(c, b, _) =>
				feval(Condition(c, Seq(b, While(c, b, NoPosition), 
														NoPosition), Empty, NoPosition), store)
			case StaticCall(clazz, mthd, argList, _) => 
				val args = fevalArgs(argList, store)
				val method = pgm.getClass(clazz).get.getMethod(mthd).get
        val argPairs = argList zip args
				val argParamPair = argPairs zip method.args
				val newStore = store.newStore(argParamPair)
				feval(method.expr, newStore)
			case DynamicCall(obj, mthd, argList, _) => 
				store.get(obj) match{
					case Some(CTValue(ObjectValue(mobj))) =>
						val args = fevalArgs(argList, store)
						val method = pgm.getClass(mobj.clazz).get.getMethod(mthd).get
            val argPairs = argList zip args
    				val argParamPair = argPairs zip method.args
						mobj.store = mobj.store.addEnv(argParamPair, store)
						feval(method.expr, mobj.store)	
					case _ => throw new Error("Object values does not exist")
				}
			case This(clazz, mthd, argList, _) => 
				val args = fevalArgs(argList, store)
				val method = pgm.getClass(clazz).get.getMethod(mthd).get
        val argPairs = argList zip args
				val argParamPair = argPairs zip method.args
				val newStore = store.newStore(argParamPair)
				feval(method.expr, newStore)
			case invoke @ Invoke(cname, mname, args, _) => 
				val (c, store1) = feval(cname, store)
				val (m, store2) = feval(mname, store1)
				(c, m) match{
					case (Constant(x: MString, _), Constant(y: MString, _)) =>
						feval(StaticCall(ClassName(x.value, NoPosition), y.value,
												args, NoPosition), store2)
					case _ => (invoke, store2)
				}
			case New(cname, argList, _) => 
				val args = fevalArgs(argList, store)
				val clazz = pgm.getClass(cname).get
        val argPairs = argList zip args
				val argParamPair = argPairs zip clazz.args
				val newStore = store.newStore(argParamPair)
				var (newConst, constStore) = feval(clazz.body.const, newStore)
        val objStore = 
            constStore.addEnvToSelf(clazz.body.vars, constStore)
				(ObjectValue(MObject(cname, objStore)), store)
			case Return(expr, _) => feval(expr, store)
			case Empty | Semi => (Empty, store)
			case _ => throw new Error(expr + 
                      "  Print, CT, RT, and IsCT cannot appear here")
		}
	}
  
  def peval(expr: Expression, store: ConstraintStore): 
                  (Expression, PEValue, ConstraintStore) = {
    return null;
  }
  
  
	private def doOp(op: Operation, s1: PEKnownValue, s2: PEKnownValue): Expression ={
		(s1.getExpr, s2.getExpr) match{
			case (v1: Constant, v2: Constant) =>
				val x = v1.value
				val y = v2.value
				doOp(op, x, y)
			case _ => Binary(op, s1.getExpr, s2.getExpr, NoPosition)
		}
	}
	
	
	private def doOp(op: Operation, x: Premitive, y: Premitive): Constant = {
		val result = op match {
			case Add =>
				(x, y) match{
					case (MInt(a), MInt(b)) => 
						MInt(a + b)
					case (MString(_), MInt(_))
						 | (MInt(_), MString(_)) 
						 | (MString(_), MString(_)) => 
						MString("" + x.optionValue.get + y.optionValue.get)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Sub =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MInt(a - b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Mul =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MInt(a * b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Div =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MInt(a / b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Eq => MBool(x.optionValue.get == y.optionValue.get)
			case Neq => MBool(x.optionValue.get != y.optionValue.get)
			case Lt =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MBool(a < b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Gt =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MBool(a > b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
			case Mod =>
				(x, y) match{
					case (MInt(a), MInt(b)) => MInt(a % b)
					case _ => throw new Error("" + x + y +" Never happens")
				}
		}
		Constant(result, NoPosition)
	}
  
	private def fevalArgs(args: List[Expression], store: ConstraintStore) :
																				List[CTValue] = {
		val evalArgs = for(arg <- args) yield{
      CTValue(feval(arg, store)._1)
    } 
		evalArgs
	}
	
  private def addArgsToStore(args: List[Var], 
              store: ConstraintStore = new ConstraintStore()
              ): ConstraintStore = {
    var temp = store
    var tail = args
    while(tail != Nil){
      val head = tail.head
      tail = tail.tail
      temp = store.add(head, Bottom)
    }
    temp
  }
}