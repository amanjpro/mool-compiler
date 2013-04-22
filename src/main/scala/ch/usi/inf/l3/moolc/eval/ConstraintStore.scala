/*
 * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */


package ch.usi.inf.l3.moolc.evaluator

import _root_.ch.usi.inf.l3.moolc.ast._


/**
 * An immutable data structure to represent a constraint variable
 *
 * @constructor takes a Var as a constructor, which states the name of 
 * the variable which is bound to this value, and its name
 * @param n a name of this instance, which will be shown in SMT solver
 * @param constraint the constraints of this instance, defaults to null
 * @param values a list of all possible memory locations that hold the values
 * of this instance, defaults to Nil
 */
class CBValue(n: String, constraint: com.microsoft.z3.BoolExpr,
                  values: List[Int]) {

// constraint = (value == 1 && b > 2) || (value == 2 && b <3)
  def this(n: String){
    this(n, null, Nil)
  }

  def this(n: String, v: Int) = {
    this(n)
    this.value_=(v)
  }
  type SMTExpr = com.microsoft.z3.Expr
  type ConstraintValue = com.microsoft.z3.BoolExpr

  private[evaluator] val smt = new SMTFrontend

  import smt._
  private[evaluator] val name = int(n)



  /**
   * A default setter for the value of this object. This method expects the 
   * value to be of type <code>PEValue</code> not a constraint.
   * 
   * @param v the location int the memory of the value of this 
   * Constraint Object.
   * @return a new instance of CBValue which holds the given value only, the 
   * new instances constraint is null
   */

  def value_= (v: Int): CBValue = {
    new CBValue(n, null, List(v))
  }

  /**
   * This method solves the Constraints for this object, and returns Some
   * result of this constraint if there is one and only one solution, 
   * None otherwise.
   * 
   * @return Some result if there is only one solution for the constraint, None 
   * otherwise.
   */
  def value: Option[Int] = {
    value(constraint)
  }

  /**
   * Please refer to CBValue.value(ConstraintValue): List[Int]
   */
  private def value(constraint: ConstraintValue): Option[Int] = {
              values match{
      case Nil => None
      case (x :: Nil) => Some(x)
      case _ => {
        constraint match{
          case null => None
          case _ => 
            val value = smt.getValue(name, constraint)
            value match{
              case -1 => None
              case x: Int => Some(values(x))
            }
        }
      }
    }
  }

  /**
   * This methods solves the Constraints (several times) for this object, 
   * and returns all possible solutions.
   * 
   * @return a list of all possible solutions
   */
  def possibleValues(): List[Int] = {
    possibleValues(constraint)
  }

  /**
   * Please refer to CBValue.possibleValues(): List[Int]
   */
  private def possibleValues(constraint: ConstraintValue): List[Int] = {
    values match{
      case Nil => Nil
      case (x :: Nil) => List(x)
      case _ => {
        constraint match{
          case null => Nil
          case _ => smt.getValues(name, constraint)
        }
      }
    }
  }

  /**
   * This method solves the Constraints for this object, and returns Some result
   * of the constraint if there is one and only one solution, None otherwise.
   * 
   * @param append the constraint to be appended to this object's constraints,
   * this way we can support conditionals, for example:
   * {{{
   * if(a == 1)
   *  b = 1
   * else
   *  b = 2
   * // The constraints of b is ((a == 1 && b == 1) || (!(a == 1) && b ==2))
   * 
   * if(a == 4)
   *  print b // the solver knows that if the execution reaches here, 
   *          // then b should be 2
   * // 
   * }}}
   * @return Some result if there is only one solution for the constraint, None 
   * otherwise.
   */
  def value_=(append: ConstraintValue): Option[Int] = {
    enterScope
    val constTemp = and(constraint, append)
    val temp2 = value(constTemp)
    leaveScope
    temp2
  }

  /**
   * Add constraint constraint value
   * 
   * @param cnstrnt This values constraint
   * @param values a list of possible locations in the memory
   */
  def value_= (cnstrnt: ConstraintValue, values: List[Int]) = {
    // constraint = cnstrnt
    // this.values = values
    /*
    TODO: Here you should find a way to easily:
    1- evaluate cond, and convert it to a constraint
    2- find the not(cond) constraint
    3- and evaluate the values
    TODO: Consider this case too:
    if(a == 2) b = 1
    else if (a > 6) b = 2
    else if(a > 2) b = 5
    else b = 4

    b = (a == 2 && b == 1) || (a > 6 && b == 2) || (a > 2 && b == 5) || 

    (!(a == 2) && !(a > 6) && !(a > 2) && b == 4)
    */
  }
}

/**
 * A simple class to imitate SSA in ConstraintStore
 * 
 * @constructor Takes three parameters, a name of the variable, the index of 
 * the variable and the version of the variable
 * 
 * 
 * TODO SSAVar, should have something like STORE ID, and the store ID should be
 * Unique per score! (this is for preventing data to be shared between 
 * two stores?)
 */
class SSAVar(val nme: String, val index: Int, val version: Int){
  /**
  * The long name of the SSAVar, this name is used for the SMT solver
  */
  val name = nme + "_" + index + "_" + version
}

/**
 * This class represents a store of variables (or environment) in a given scope.
 * Please be warned that this class is immutable.
 *
 * The variables are stored in the form of one of the following:
 * <ul>
 * <li> A PEValue (if they have one constant value during compilation)</li>
 * <li> A constraint value in the form of: 
 *       (&alpha; == PEValue(x) &and; &beta; == PEValue(y))
 * <li> Top, Bottom values denoting that the value is not known yet!
 * </ul>
 * 
 * This store makes sure to implement the following concepts:
 * <ul>
 * <li> SSA: Single-Static Assignment</li>
 * <li> Full support for Objects! (it has two Environments, 
 * one from variables to locations. The other one from locations to
 *  values!)</li>
 * </ul>
 * 
 * Having two Environments from variables to locations, and from locations to
 * values, guarantees the correctness of the following scenario:
 * {{{
 * class A(var value: Int)
 * 
 * var b = new A(3)
 * var a = b
 * 
 * a.value = 4
 * 
 * // Now b.value should also be 4!
 * }}}
 * @param varVersions Maps a variable to its current version .Variables 
 * version updates, if we update its value:
 * {{{
 * var x = 2 // x has version 0
 * x = 3 // x's version is updated to 1
 * }}}
 * @param env Maps a Var to its location.
 * @param memory Maps a location to the value store in the location
 * @param memoryMap The SMT solver that we are using does not accept complex 
 * object, so we had map each value to an index, and use the index for 
 * solving the constraints.
 * @param location the next available location in memory map
 * @param next the next available variable index for SMT
 */

class ConstraintStore private (private val varVersions: Map[Var, SSAVar],
                                private val env: Map[Var, Int],
                                private val memory: Map[Int, CBValue],
                                private val memoryMap: Map[Int, PEValue],
                                private val location: Int,
                                private val next: Int){

  def this(){
    this(Map.empty, Map.empty, Map.empty, Map.empty, 0, 0)
  }

  type Location = Int

  // b = "3"
//   
//   
//   else 
//   
//   b = "4"
//   
//   
//   if (b.equals("5"))      {b!=null,b.equals("5")}
//     a = 10        {b.equals("5"),a==10}
//     f(b)          
//   else
//     a = 20        {!b.equals("5"),a==20}
//     
//   f(x) {                {x>0}
//     if (x < 0)
//       error()           unreachable  
//     print(x)            ...
//   }
  
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
   * Each version of a variable has its unique name in the SMT, this method
   * produces the name for a variable based on its version.
   * 
   * @param v the variable in the question
   * @return Some SMT name of the variable, if the variable v is already in the 
   * environment, None otherwise
   */
  private def getSMTName(v: Var): Option[String] = {
    varVersions.get(v) match {
      case Some(ssa) => Some(ssa.name)
      case _ => None
    }
  }

  /**
   * Returns the binding of the variable v if there is one, None otherwise
   *
   * @param a variable to look for its value
   * @return Some value of the current variable if there is one, None otherwise
   */
  def get(v: Var): Option[PEValue] = {
    env.get(v) match{
      case Some(x) =>{
        val cbval = memory(x)
        val result = cbval.value
        memoryMap.get(x)
      }
      case None => None
    } 
  }

  /**
   * Updates the current version of the given variable
   * 
   * @param v a variable to be looked for its SSAVar
   * @return a tuple of the updated SSVar of the given variable and the next
   * available variable index for SMT
   */
  private def updateVersion(v: Var): (SSAVar, Int) = {
    varVersions.get(v) match{
      case Some(x) => 
        (new SSAVar(v.name, x.index, x.version + 1), next)
      case None => 
        val index = next+1
        (new SSAVar(v.name, next, 0), index)
    }
  }

  /**
   * Adds the bindings for the given variable
   * 
   * If a variable is already bounded, then a new variable with the same name
   * but a different index will be bounded. This way, this store can easily 
   * imitate SSA.
   * 
   * @param v1 a variable to be bounded
   * @param v2 a variable to be bounded to
   * @return a new constraint store, which has the bindings of variable v1 added
   */
  def add(v1: Var, v2: Var): ConstraintStore = {
    val (ssa, nxt) = updateVersion(v1)
    val loc = env(v2)
    env + (v1 -> loc)
    new ConstraintStore(varVersions + (v1 -> ssa),
                         env + (v1 -> loc),
                         memory, memoryMap,
                         location,
                         nxt)
  }


  /**
   * Adds the bindings for the given variable.
   * 
   * If a variable is already bounded, then a new variable with the same name
   * but a different index will be bounded. This way, this store can easily 
   * imitate SSA.
   * 
   * @param v a variable to be bounded
   * @param p the value of the variable
   * @return a new constraint store, which has the bindings of variable v1 added
   */

  def add(v: Var, p: PEValue): ConstraintStore = {
    val (ssa, nxt) = updateVersion(v)
    val loc = location + 1
    val envTemp = env + (v -> location)

    val cbvalue = new CBValue(ssa.name, location)
    val memoryTemp = memory + (loc -> cbvalue)
    val memoryMapTemp = memoryMap + (loc -> p)

    new ConstraintStore(varVersions + (v -> ssa),
                         envTemp,
                         memoryTemp, memoryMapTemp,
                         loc,
                         nxt)
  }

  /**
   * Removes a variable from store
   * 
   * @param v the variable to be removed
   * @return a new constraint store, which has the bindings of variable v
   * removed
   */
  def remove(v: Var): ConstraintStore = {
    val varVersionsTemp = varVersions - (v)
    val envTemp = env - (v)
    new ConstraintStore(varVersionsTemp,
                         envTemp,
                         memory, memoryMap,
                         location,
                         next)
  }

  private def getPEValue(v: Var): Option[PEValue] = {
    env.get(v) match{
      case Some(loc) => memoryMap.get(loc)
      case _ => None
    }
  }

  def isCT(v: Var): Boolean = {
    getPEValue(v) match{
      case Some(CTValue(_)) => true
      case _ => false
    }
  }

  def isRT(v: Var): Boolean = {
    getPEValue(v) match{
      case Some(Top) => true
      case _ => false
    }
  }

  /**
   * Creates a new store with the given env
   * 
   * @param env a new environment to be added
   * @return a new Constraint store, having only the given env
   */

  def newStore(env: Map[Var, PEValue]): ConstraintStore = {
    new ConstraintStore().addEnv(env)
  }

  /**
   * Creates a new store with the given env added
   * 
   * @param env a new environment to be added
   * @return a new Constraint store, having the given env, and the 
   * current one
   */
  def addEnv(pairs: Map[Var, PEValue]): ConstraintStore = {
    var storeTemp = this
    var head: (Var, PEValue) = null
    var tail = pairs
    while(tail != Map.empty){
      head = tail.head
      tail = tail.tail
      storeTemp = storeTemp.add(head._1, head._2)
    }
    storeTemp
  }
  
  /**
   * A very important method for handling function calls on objects accurately,
   *  this makes sure to correctly pass all the parameter values 
   * (or psudovalues -- aka constraint values) to the called method while
   * still preserving the store of the object.
   * 
   * @param params a list of tuple of method arguments (as a pair of 
   * expressions and PEValues)
   * @param sourse the source constraint store, to look for variables
   * psudovalues from
   * @return a new store which has all the "possible" informations of its 
   * parameters
   * 
   * 
   * TODO: RE-THINK ABOUT PASSED PARAMETERS IF THEY WERE EXPRESSIONS. 
   * you can compute the expressions dependencies, and build more 
   * constraints for them?
   */
  def addEnv(vars: List[((Expression, PEValue), Var)], 
                      source: ConstraintStore): ConstraintStore = {
    var tail = vars
    var tempStore = this
    while(tail != Nil){
      val  head = tail.head
      tail = tail.tail
      head._1 match{
        case (x: Var, _) =>
          val ssa = source.varVersions(x)
          val l = source.env(x)
          val cbvalue = source.memory(l)
          val value = source.memoryMap(l)
          val loc = tempStore.location+1
          val nxt = tempStore.next+1
          tempStore = 
            new ConstraintStore(tempStore.varVersions + (head._2 -> ssa),
                                          tempStore.env + (head._2 -> l),
                                          tempStore.memory + (l -> cbvalue),
                                          tempStore.memoryMap + (l -> value),
                                          loc,
                                          nxt)
        case (_, x) => tempStore = tempStore.add(head._2, x)
      }
    }
    
    tempStore
  }
  
  
  /**
   * A very important method for handling function calls on objects accurately,
   *  this makes sure to correctly pass all the parameter values 
   * (or psudovalues -- aka constraint values) to the called method while
   * still preserving the store of the object.
   * 
   * @param params a list of tuple of method parameters
   * @param sourse the source constraint store, to look for variables
   * psudovalues from
   * @return a new store which has all the "possible" informations of its 
   * parameters
   */
  def addEnvToSelf(vars: List[Var], 
                      source: ConstraintStore): ConstraintStore = {
    var tail = vars
    var tempStore = this
    while(tail != Nil){
      val  head = tail.head
      tail = tail.tail
      val ssa = source.varVersions(head)
      val l = source.env(head)
      val cbvalue = source.memory(l)
      val value = source.memoryMap(l)
      val loc = tempStore.location+1
      val nxt = tempStore.next+1
      tempStore = new ConstraintStore(tempStore.varVersions + (head -> ssa),
                                      tempStore.env + (head -> l),
                                      tempStore.memory + (l -> cbvalue),
                                      tempStore.memoryMap + (l -> value),
                                      loc,
                                      nxt)
    }
    
    tempStore
  }
  
  /**
   * A very important method for handling function calls accurately, this makes
   * sure to correctly pass all the parameter values (or psudovalues -- aka 
   * constraint values) to the called method.
   * 
   * @param params a list of tuple of method arguments (as a pair of 
   * expressions and PEValues)
   * and parameters
   * @return a new store which has all the "possible" informations of its 
   * parameters
   * 
   * 
   * 
   * TODO: RE-THINK ABOUT PASSED PARAMETERS IF THEY WERE EXPRESSIONS. 
   * you can compute the expressions dependencies, and build more 
   * constraints for them?
   */
  def newStore(vars: List[((Expression, PEValue), Var)]): ConstraintStore = {
    var tail = vars
    var tempStore = new ConstraintStore
    var next = -1
    var location = -1
    while(tail != Nil){
      val head = tail.head
      tail = tail.tail
      head._1 match{
        case (x: Var, _) =>
          val ssa = varVersions(x)
          val l = env(x)
          val cbvalue = memory(l)
          val value = memoryMap(l)
          location = if(location < l) l else location
          next = if(next < ssa.index) ssa.index else next
          location+=1
          next+=1
          tempStore = 
            new ConstraintStore(tempStore.varVersions + (head._2 -> ssa),
                                          tempStore.env + (head._2 -> l),
                                          tempStore.memory + (l -> cbvalue),
                                          tempStore.memoryMap + (l -> value),
                                          tempStore.location,
                                          tempStore.next)
        case (_, x) => tempStore = tempStore.add(head._2, x)
      }
    }
    
    tempStore
  }

  /**
   * A silly method, does nothing just returns this
   */
  def cloneStore() = this

  override def toString  = 
        varVersions.toString + "\n" + env.toString	+ "\n" + memory.toString
}
