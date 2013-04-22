/*
 * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

 
package ch.usi.inf.l3.moolc.evaluator

import _root_.ch.usi.inf.l3.moolc.ast._


class Store extends StoreTrait{
	def newStore(env: Map[Var, PEValue]) = {
		val store = new Store
		store.env = env
		store
	}
	
	def addEnv(env: Map[Var, PEValue]) = {
		this.env = this.env ++ env
	}
	private var env: Map[Var, PEValue] = Map.empty
	 
	/**
	* Returns the binding of the variable v
	*/
	def get(v: Var): Option[PEValue] = {
		env.get(v)
	}
	 
	def add(v: Var, p: PEValue) = {
		env = env + (v -> p)
	}
	
	def remove(v: Var) = {
		env = env - (v)
	}
	
	def isCT(v: Var): Boolean = {
		env.get(v) match{
			case Some(CTValue(_)) => true
			case _ => false
		}
	}
	
	def isRT(v: Var): Boolean = {
		env.get(v) match{
			case Some(Top) => true
			case _ => false
		}
	}
	
	private def makeConsistent(ml: Map[Var, PEValue], 
                                ms: Map[Var, PEValue]): Unit = {
		for((k, v) <- ml) {
			if(v == Top){
				add(k, Top)
			}
			else{
				ms.get(k) match{
					case Some(x) =>
						if(x == v) add(k, v)
						else if(x == Top) add(k, Top)
						else add(k, Bottom)
					case None => add(k, Bottom)
				}
			}
		}
	}
	def makeConsistent(st2: Store, st3: Store): Unit = {
    (st2, st3) match{
      case (s2: Store, s3: Store) =>
    		val map2 = s2.env
    		val map3 = s3.env
    		if(map2.size > map2.size) makeConsistent(map2, map3)
    		else makeConsistent(map2, map3)
      case _ => 
    }
	}
	def cloneStore() = newStore(env)
	override def toString()  = env.toString	
}

