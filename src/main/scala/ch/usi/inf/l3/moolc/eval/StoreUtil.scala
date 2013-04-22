/*
 * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

 
package ch.usi.inf.l3.moolc.evaluator

import _root_.ch.usi.inf.l3.moolc.ast._

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