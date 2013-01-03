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

class Evaluator(pgm: Program){
	
	def start(pe: Boolean = false) = {
		if(!pe){
			for(clazz <- pgm.classes) {
				feval(clazz.body.const, clazz, pgm)
				for(method <- clazz.body.methods){
					feval(method.expr, method, pgm)
				}
			}
		}
	}
	private def evalArgs(args: List[Expression], block: Block, program: Program): 
																			Option[List[Value]]= {
		val eargsOpt = for(arg <- args) yield feval(arg, block, program)
		val eargs = for((arg, _) <- eargsOpt) yield arg.get
		if(eargs.contains(None)) None
		else Some(eargs)
	}
	
	private def find(classes: List[MClass], cname: ClassName): Option[MClass] = {
		val r = classes.filter(_.name == cname)
		r.size match{
			case 0 => None
			case _ => Some(r.head)
		}
	}
	
	private def find(methods: List[MMethod], mname: String): Option[MMethod] = {
		val r = methods.filter(_.name == mname)
		r.size match{
			case 0 => None
			case _ => Some(r.head)
		}
	}
	
	private def doOpInt[B](op: Operation, x: MInt, y: MInt): Value = {
		op match{
			case Add => MInt(x.optionValue.get + y.optionValue.get)
			case Sub => MInt(x.optionValue.get - y.optionValue.get)
			case Mul => MInt(x.optionValue.get * y.optionValue.get)
			case Div => MInt(x.optionValue.get / y.optionValue.get)
			case Eq => MBool(x.optionValue.get == y.optionValue.get)
			case Neq => MBool(x.optionValue.get != y.optionValue.get)
			case Lt => MBool(x.optionValue.get < y.optionValue.get)
			case Gt => MBool(x.optionValue.get > y.optionValue.get)
			case Mod => MInt(x.optionValue.get % y.optionValue.get)
		}
	}
	
	private def doOpString[B](op: Operation, x: MString, y: MString): Value = {
		op match{
			case Add => MString(x.optionValue.get + y.optionValue.get)
			case Eq => MBool(x.optionValue.get == y.optionValue.get)
			case Neq => MBool(x.optionValue.get != y.optionValue.get)
			case Lt => MBool(x.optionValue.get < y.optionValue.get)
			case Gt => MBool(x.optionValue.get > y.optionValue.get)
			case _ => Null
		}
	}
	
	private def doOpBool[B](op: Operation, x: MBool, y: MBool): Value = {
		op match {
			case Eq => MBool(x.optionValue.get == y.optionValue.get)
			case Neq => MBool(x.optionValue.get != y.optionValue.get)
			case Lt => MBool(x.optionValue.get < y.optionValue.get)
			case Gt => MBool(x.optionValue.get > y.optionValue.get)
			case _ => Null
		}
	}
	
	def feval(expr: Expression, block: Block, program: Program): 
							                          (Option[Value], Block) ={
		expr match{
			case Constant(c, _) => 
				// a dirty hack to fix a design problem that 
				// Primitive has no relation with Value (although their childs,
				// MString, MBool, MInt and Null are inhering both)
				c match{
					case MString(x) => (Some(MString(x)), block)
					case MInt(x) => (Some(MInt(x)), block)
					case MBool(x) => (Some(MBool(x)), block)
					case Null => (Some(Null), block)
				}
			case Binary(op, x, y, _) => {
				val (exp1, _) = feval(x, block, program)
				val (exp2, _) = feval(x, block, program)
				(exp1, exp2) match{
					case (Some(a: MInt), Some(b: MInt)) => {
						(Some(doOpInt(op, a, b)), block)
					}
					case (Some(a: MString), Some(b: MString)) => {
						(Some(doOpString(op, a, b)), block)
					}
					case (Some(a: MBool), Some(b: MBool)) => {
						(Some(doOpBool(op, a, b)), block)
					}
					case (_, _) => (None, block)
				}
			}
			case x @ Var(_, _, _, _) => (block.env.get(x), block)
			case Assign(x, exp1, _) => {
				val (assign, _) = feval(exp1, block, program) 
				assign match{
					case v @ Some(a) => {
						block.env = block.env + (x -> a) 
						(v, block)
					}
					case _ => (None, block)
				}
			}
			case Seq(exp1, exp2, _) => {
				val (_, block2) = feval(exp1, block, program)
				feval(exp2, block2, program)
			}
			case Condition(cond, t, f, _) => {
				val (r, _) = feval(cond, block, program) 
				r match {
					case Some(MBool(true)) => feval(t, block, program)
					case Some(MBool(false)) => feval(f, block, program)
					case _ => (None, block)
				}
			}
			case loop @ While(cond, body, _) => {
				feval(Condition(cond, Seq(body, loop, NoPosition), 
																		Empty, NoPosition), block, program)
			}
			case DynamicCall(objName, method, args, _) => {
				val objct = block.env(objName)
				objct match {
					case obj @ MObject(_, _) => {
						val eargsOpt = evalArgs(args, block, program)
						eargsOpt match {
							case Some(eargs) => {
								val oMethod = find(obj.clazz.body.methods, method)
								oMethod match {
									case Some(MMethod(mod, name, tpe, margs, pos, e, menv)) => {
										val nenv = eargs zip margs
									  val nenvMap = Map(nenv.map(_.swap): _*)
										feval(e, 
											MMethod(mod, name, tpe, margs, pos, e, 
																	menv ++ nenvMap ++ obj.env), program)
									}
									case None => (None, block)
								}
							}
							case None => (None, block)
						}
					}
					case _ => (None, block)
				}
			}
			case StaticCall(cname, method, args, _) => { 
				//this is only good for static methods
				val eargsOpt = evalArgs(args, block, program)
				eargsOpt match {
					case Some(eargs) => {
						val oClass = find(program.classes, cname)
						oClass match{
							case Some(clazz: MClass) => {
								val oMethod = find(clazz.body.methods, method) 
								oMethod match {
									case Some(MMethod(mod, name, tpe, margs, pos, e, menv)) => {
										val nenv = eargs zip margs
									  val nenvMap = Map(nenv.map(_.swap): _*)
										feval(e, 
											MMethod(mod, name, tpe, margs, pos,
																e, menv ++ nenvMap), program)
									}
									case _ => (None, block)
								}
							}
							case _ => (None, block)
						}
					}
					case None => (None, block)
				}
				
			}
			case Invoke(cname, mname, args, _) => {
				val (opt1, _) = feval(cname, block, program)
				val (opt2, _) = feval(mname, block, program)
				(opt1, opt2) match{
					case (Some(x: MString), Some(y: MString)) => 
						feval(StaticCall(ClassName(x.value, NoPosition), 
												y.value, args, NoPosition), block, program)
					case (_, _) => (None, block)
				}
				
			}
			case New(cName, args, _) => {
				val eargsOpt = evalArgs(args, block, program)
				eargsOpt match{
					case Some(eargs) => {
						val oClass = find(program.classes, cName)
						oClass match{
							case Some(clazz: MClass) => {
								val nenv = eargs zip clazz.args
							  val nenvMap = Map(nenv.map(_.swap): _*)
								val nclass = MClass(clazz.name, clazz.args, 
													clazz.body, NoPosition, nenvMap)
								val (_, newBlock) = feval(clazz.body.const, nclass, program)
								val r = MObject(nclass, newBlock.env)
								(Some(r), block)
							}
							case _ => (None, block)
						}
					}
					case None => (None, block)
				}	
			}
			case CT(expr, _, _) => feval(expr, block, program)
			case IsCT(expr, _) => (Some(MBool(true)), block)
			case RT(expr, _) => feval(expr, block, program)
			case _ => (None, block)
		}
	}
}

