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
 

/**
 * A datastructure to represent a constraint value
 *
 * @constructor takes a Var as a constructor, which states the name of 
 * the variable which is bound to this value, and an integer, to index
 * this variable.
 */
class CBVar(val v: Var, val index: Int){
  type SMTExpr = com.microsoft.z3.Expr
  type ConstraintValue = com.microsoft.z3.BoolExpr
  
  private val smt = new SMTFrontend
  
  import smt._
  private val const = int(v.name+"_"+index)
  private var constraint: ConstraintValue = null
  private var values: List[PEValue] = Nil
  
  def setValue(x: PEValue) = {
    values = x :: values
  }
  
  def getValue(): Option[PEValue] = {
    values match{
      case Nil => None
      case (x::Nil) => Some(x)
      case _ => {
        val value = smt.getValue(const, constraint)
        value match{
          case -1 => None
          case x: Int => Some(values(x))
        }
      }
    }
  }
  
  def setConstraint(cond: Expression, 
    tbranch: Expression, fbranch: Expression) = {
    /*
     TODO: Here you should find a way to easily:
       1- evaluate cond, and convert it to a constraint
       2- find the not(cond) constraint
       3- and evaluate the values
     */
  }
}

/**
 * This class represtens the store of the variables (or environtment) 
 * in a given scope.
 *
 * The variables are stored in the form of one fo the following:
 * <ul>
 * <li> A PEValue (if they have one constant value during compilation)</li>
 * <li> A constraint value in the form of: 
 *       (&alfa; == PEValue(x) &and; &beta; == PEValue(y))
 * <li> Top, Bottom values denoting that the value is not known yet!
 * </ul>
 */
class ConstraintStore {
  
  private var nesting: ConstraintStore = null
  
  /** 
   * Maps a variable to its constraint value.
   */
  private var env: Map[Var, CBVar] = Map.empty
  
  /**
   * This map is to make the compiler a bit more efficient. It stores
   * each variable with a list of its dependants, for example in the 
   * following code, the value of <code>b</code> depends on the value of 
   * <code>a</code>:
   * {{{
   * val b = if(a == 4) then 3 else 4
   * }}}
   */
  private var dependants: Map[Var, List[Var]] = Map.empty
    
  private var next: Int = 0
  
  
  /**
   * Creates a nested store for the current store
   */
  // def getNestedStore() = {
//     val store = new Store(env, smtVars, next, pc, this)
//     enterScope
//     store
//   }
  
  
  /** 
   * Exits this store, and returns the nesting store
   * 
   * The instance which calls this method <strong>MAY NOT</strong>
   * be used anymore.
   */
  // def exitStore() = {
//     leaveStore
//     parent
//   }
  
  
  /**
   * Returns the binding of the variable v if there is one, None otherwise
   *
   * @param a variable to look for its value
   */
  def get(v: Var): Option[PEValue] = {
    env.get(v) match{
      case Some(x) => x.getValue
      case None => None
    }
  }
   
  def add(v: Var, p: PEValue) = {
    // // Check if we already have this values stored
//     val (list, isNew) = env.get(v) match{
//       case None => (Nil, true)
//       case Some(list) => (list, false)
//     }
//     
//     val newVals = list ++ List(p)
//     env = env + (v -> newVals)
//     
//     val expr = isNew match {
//                   case true => 
//                     val vExpr = int(v.name+"_"+next)
//                     next += 1
//                     smtStore = smtStore + (v -> vExpr)
//                     vExpr
//                   case false => smtStore(v)
//                 }
//                 
//      
//     /*
//       FIXME
//       When you re-intialize a variable, your
//       previous constraints about this variable should be 
//       invalidated! (BUT HOW?)
//     */
//     
//     val v_eq_pe = equ(expr, newVals.size - 1) // v == pe
//     pc = and(pc, v_eq_pe) // pc && (v == pe)
  }
  
  def remove(v: Var): Unit = {
    env = env - (v)
    val x = dependants.get(v)
    dependants = dependants - (v)
    x match{
      case Some(list) => list.foreach(remove(_))
      case None =>
    }
  }
  
  // def isCT(v: Var): Boolean = {
 //    env.get(v) match{
 //      case Some(CTValue(_)) => true
 //      case _ => false
 //    }
 //  }
 //  
 //  def isRT(v: Var): Boolean = {
 //    env.get(v) match{
 //      case Some(Top) => true
 //      case _ => false
 //    }
 //  }
 //  
 //  private def makeConsistent(ml: Map[Var, PEValue], ms: Map[Var, PEValue]): Unit = {
 //    for((k, v) <- ml) {
 //      if(v == Top){
 //        add(k, Top)
 //      }
 //      else{
 //        ms.get(k) match{
 //          case Some(x) =>
 //            if(x == v) add(k, v)
 //            else if(x == Top) add(k, Top)
 //            else add(k, Bottom)
 //          case None => add(k, Bottom)
 //        }
 //      }
 //    }
 //  }
 //  def makeConsistent(s2: Store, s3: Store): Unit = {
 //    val map2 = s2.env
 //    val map3 = s3.env
 //    if(map2.size > map2.size) makeConsistent(map2, map3)
 //    else makeConsistent(map2, map3)
 //  }

	override def toString  = env.toString	
}

class Store {
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
	
	private def makeConsistent(ml: Map[Var, PEValue], ms: Map[Var, PEValue]): Unit = {
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
	def makeConsistent(s2: Store, s3: Store): Unit = {
		val map2 = s2.env
		val map3 = s3.env
		if(map2.size > map2.size) makeConsistent(map2, map3)
		else makeConsistent(map2, map3)
	}
	def cloneStore = newStore(env)
	override def toString  = env.toString	
}

class MethodBank{
	private var methods: List[MMethod] = Nil
	private var specializedMethods:
			 			Map[(String, List[PEValue]), MMethod] = Map.empty
	private var nextMethodID = 0
	private def nullify(args: List[PEValue]) = {
		var temp: List[PEValue] = Nil
		for(arg <- args) {
			arg match{
				case x: CTValue => temp = x :: temp
				case _ => temp = Bottom :: temp
			}
		}
		temp.reverse
	}
	
	def getSpecializedMethodsList = methods
	def getMethodName(base: String) = {
		val newName = base + "_" + nextMethodID
		nextMethodID += 1
		newName
	}
	
	def add(name: String, args: List[PEValue], method: MMethod) = {
		methods = method :: methods
		specializedMethods = specializedMethods + ((name, nullify(args)) -> method)
	}
	
	def get(name: String, args: List[PEValue]): MMethod = {
		println(args)
		specializedMethods((name, nullify(args)))
	}
	
	def getOption(name: String, args: List[PEValue]) : Option[MMethod] = {
		specializedMethods.get((name, nullify(args)))
	}
	
	def has(name: String, args: List[PEValue]) : Boolean = {
		getOption(name, args) match {
			case Some(x) => true
			case None => false
		}
	}
}
class ClassBank{
	private var nextClassID = 0
	
	private var classes: List[MClass] = Nil
	
	def getAllSpecializedClasses = classes
	
	private def nullify(args: List[PEValue]) = {
		var temp: List[PEValue] = Nil
		for(arg <- args) {
			arg match{
				case x: CTValue => temp = x :: temp
				case _ => temp = Bottom :: temp
			}
		}
		temp.reverse
	}
	
	def getClassName(base: ClassName) = {
		val newName = base.name + "_" + nextClassID
		nextClassID = nextClassID + 1
		ClassName(newName, NoPosition)
	}
	private var specializedClasses: 
										Map[(ClassName, List[PEValue]), MClass] = Map.empty
	def add(cname: ClassName, args: List[PEValue], clazz: MClass) = {
		classes = clazz :: classes
		specializedClasses = specializedClasses + ((cname, nullify(args)) -> clazz)
	}
	
	def get(cname: ClassName, args: List[PEValue]): MClass = {
		specializedClasses((cname, nullify(args)))
	}
	
	def getOption(cname: ClassName, args: List[PEValue]) : Option[MClass] = {
		specializedClasses.get((cname, nullify(args)))
	}
	
	def has(cname: ClassName, args: List[PEValue]) : Boolean = {
		getOption(cname, args) match {
			case Some(x) => true
			case None => false
		}
	}
}
/*
 * Represents an compile-time value
 */
case class CTValue(expr: Expression) extends PEValue with PEKnownValue {
	def getExpr = expr
}

/*
 * Represents a Runtime value, ⟂
 */
case object Top extends PEValue{}
	
/*
 * Represents an unknown value, T
 */
case object Bottom extends PEValue{}
	
/*
 * Represents an abstract value
 */
case class AbsValue(expr: Expression) extends PEValue with PEKnownValue {
	def getExpr = expr
}

 
sealed trait PEValue{}
	
sealed trait PEKnownValue {
	def getExpr: Expression
}