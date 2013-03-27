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

