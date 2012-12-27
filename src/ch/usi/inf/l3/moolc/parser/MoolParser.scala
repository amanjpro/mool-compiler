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

package ch.usi.inf.l3.moolc.parser

import _root_.ch.usi.inf.l3.moolc.lexer._
import _root_.ch.usi.inf.l3.moolc.ast._

/*
 * # vs is string, vb is bool, vn is number and C:p is object
 * v = null | vs | vb | vn | C:p
 * type = int | bool | str | C
 * Prog = CD Prog | epsilon
 * params = param | param params2 , param  | epsilon
 * params2 = , param params2 | epsilon
 * param = x: type
 * vars = var x: type; vars | epsilon
 * CD = class C(params) {vars init{e} methods}
 * mod = static | method
 * methods = VoidMethod methods | TypedMethod methods | epsilon
 * VoidMethod = mod void m(params) {e}
 * TypedMethods = mod type m(params) {e return x}
 * 							| mod type m(params) {e return v}
 * op = + | - | * | / | == | != | < | > | %
 * args = e | e, e | epsilon
 *
 * e = v										# constant value
 *   | x										# variable
 *   | this									# self-reference //not implemented
 *   | C										# class name
 *   | var x: type = e			# variable declaration
 *   | x = e								# assignment
 *   | print e              # print statement
 *   | e; e									# sequence
 *   | e op e								# binary operation
 *   | if e then e else e 	# conditional
 *   | while e do e					# iteration
 *   | e.m(args)						# method invocation		
 *   | invoke(e, e, args)		# reflective method invocation
 *   | new C(args)					# constructor call
 *   | CT(e, e)							# execute at compile time
 *   | RT(e)								# execute at runtime
 *   | IsCT(e)							# tests for a compile-time value
 */

class Parser(lines: Array[String], tkns: List[(Token, Pos)]) {

	private var tokens = tkns
	// private val lines = read(file)
	// 	//Token, line number
	// 	private var tokens: List[(Token, Int)] = Nil
	private var classes: List[String] = Nil
	private var mthd: Map[String, List[String]] = Map.empty
	
	private def findClassNames: Unit ={
		var i = 0
		var list = tokens
		while(list != Nil){
			val head = list.head
			list = list.tail
			head match{
				case (Word(Keywords.Class), _) if(list != Nil) => {
					val hd = list.head
					list = list.tail
					hd match {
						case (ID(x), _) => classes = x :: classes
						case (_, _) => 
					}
				}
				case (_, _) =>
			}
		}
	}
	
	
	private def parse() = {
		checkAgainstIllegal
		findClassNames
		var classList: List[MClass] = Nil
		while(tokens != Nil) {
			val (_, p1) = tokens.head
			reduce(Word(Keywords.Class))
			val (_, p2) = tokens.head
			val cname = ClassName(reduce(), p2)
			cname :: classes
			val params = paramsList()
			reduce(OpenBrace)
			val vars = varsList()
			val init = const()
			val methods = defs(cname)
			reduce(CloseBrace)
			val body = new MClassBody(vars, init, methods)
			classList = new MClass(cname, params, body, p1, Map.empty) :: classList
		} 
		new Program(classList, Map.empty)
	}
	
	private def argsList(): List[Expression] = {
		reduce(OpenParan)
		val args = argsList2
		reduce(CloseParan)
		args
	}
	private def argsList2(): List[Expression] = {
		var more = true
		var args: List[Expression] = Nil
		while(tokens != Nil && more) {
			more = ! test(CloseParan)
			if(more) args = stmt :: args
			more = ! test(CloseParan)
			if(more) reduce(Coma)
		}
		args.reverse
	}
	
	private def paramsList(): List[Var] = {
		reduce(OpenParan)
		var more = true
		var params: List[Var] = List.empty
		while(tokens != Nil && more) {
			more = ! test(CloseParan)
			if(more){
				val (_, pos) = tokens.head
				params = Var(reduce(), reduceType, pos) :: params
			}
			more = ! test(CloseParan)
			if(more) reduce(Coma)
		}
		reduce(CloseParan)
		params.reverse
	}
	private def varsList(): List[Var] = {
		var vars: List[Var] = List.empty
		while(tokens != Nil && test(Word(Keywords.Var))){
			reduce(Word(Keywords.Var))
			val (_, pos) = tokens.head
			vars = Var(reduce(), reduceType, pos) :: vars
			reduce(Semicolon)
		}
		vars.reverse
	}
	private def const(): Expression = {
		var expr: Expression = Empty
		if(test(Word(Keywords.Init))){
			reduce(Word(Keywords.Init))
			reduce(OpenBrace)
			expr = stmts()
			reduce(CloseBrace)
		}
		expr
	}
	private def defs(cname: ClassName): List[MMethod] = {
		var mod: Modifier = InstanceMod
		var expr: Expression = Empty
		var methods: List[MMethod] = List.empty
		var methodsString: List[String] = List.empty
		while(test(Word(Keywords.Method)) || test(Word(Keywords.Static))) {
			if(test(Word(Keywords.Method))) reduce(Word(Keywords.Method))
			else {
				reduce(Word(Keywords.Static))
				mod = ClassMod
			}
			var tpe: Types = Unknown
			if(test(Word(Keywords.Void))) {
				tpe = Void
				reduce(Word(Keywords.Void))
			}
			else tpe = reduceType2
			val (_, pos) = tokens.head
			var name = reduce()
			methodsString = name :: methodsString
			val params = paramsList()
			reduce(OpenBrace)
			expr = stmts()
			if(tpe != Void) expr = Seq(expr, reduceReturn, pos)
			methods = MMethod(mod, name, tpe, params, pos, expr, Map.empty) :: methods
			reduce(CloseBrace)
		}
		// methods = methods.reverse
		mthd = mthd + (cname.name -> methodsString)
		methods
	}
	
	private def stmts(): Seq = {
		val (_, pos) = tokens.head
		val next = stmt
		if(next == Semi){
			reduce(Semicolon)
			Seq(next, stmts, pos)
		}
		else if(next == Empty) Seq(next, Empty, pos)
		else Seq(next, stmts, pos)
	}
	
	private def stmt(): Expression = stmt(false)
	
	private def stmt(revist: Boolean): Expression = {
		if(! test(CloseBrace)){
			val (token, pos) = tokens.head
			token match {
				case Semicolon => Semi
				case Word(Keywords.While) => reduceWhile
				case Word(Keywords.If) => reduceIf
				case Word(Keywords.Var) => reduceVar
				case Word(Keywords.Invoke) => reduceInvoke
				case Word(Keywords.CT) => reduceCT
				case Word(Keywords.RT) => reduceRT
				case Word(Keywords.IsCT) => reduceIsCT
				case Word(Keywords.New) => reduceNew
				case Word(Keywords.Return) => Empty
				case Word(Keywords.Print) => reducePrint
				case OpenBrace => {
					reduce(OpenBrace)
					val seq = stmts()
					reduce(CloseBrace)
					seq
				}
				case _ => {
					val (next, _) = tokens.tail.head
					next match {
						case assign @ Operator(Operators.Assign) => {
							val vr = Var(reduce(), Unknown, pos)
							reduce(assign)
							Assign(vr, stmt, pos)
						}
						case op @ Operator(x) if !revist => {
							val expr1 = stmt(true)
							reduce(op)
							var expr2 = stmt
							Binary(binary(x), expr1, expr2, pos)
						}
						case Dot if !revist=>{
							val expr1 = stmt(true)
							reduce(Dot)
							val expr2 = stmt
							val args = argsList
							(expr1, expr2) match {
								case (a: ClassName, b: Var) => {
									mthd(a.name).contains(b) match {
										case true => StaticCall(a, b.name, args, pos)
										case false => error(pos)
									}
								}
								case (a: Var, b: Var) => DynamicCall(a, b.name, args, pos)
								case _ => error(pos)
							}
						}
						case _ => {
							tokens = tokens.tail
							token match{
								case ValInt(x) => Constant(MInt(x), pos)
								case ValString(x) => Constant(MString(x), pos)
								case ValBool(x) => Constant(MBool(x), pos)
								case ValNull => Constant(Null, pos)
								case ID(x) => {
									if(classes.contains(x))
										ClassName(x, pos)
									else Var(x, Unknown, pos)
								}
								case _ => error(pos)
							}
						} 
					}
				}
			}
		}
		else Empty
	}
	
	private def reduceReturn(): Return = {
		reduce(Word(Keywords.Return))
		val (token, pos) = tokens.head
		val rtrn = token match{
			case ValInt(x) => Constant(MInt(x), pos)
			case ValString(x) => Constant(MString(x), pos)
			case ValBool(x) => Constant(MBool(x), pos)
			case ValNull => Constant(Null, pos)
			case ID(x) if(!classes.contains(x))=> Var(x, Unknown, pos)
			case _ => error(pos)
		}
		tokens = tokens.tail
		reduce(Semicolon)
		Return(rtrn, pos)
	}
	
	private def binary(op: Operators.Value): Operation = {
		op match{
			case Operators.Add => Add
			case Operators.Sub => Sub
			case Operators.Mul => Mul
			case Operators.Div => Div
			case Operators.Eq => Eq
			case Operators.Neq => Neq
			case Operators.Lt => Lt
			case Operators.Gt => Gt
			case Operators.Mod => Mod
		}
	}
	
	private def reduceNew: New = {
		val (_, p1) = tokens.head
		reduce(Word(Keywords.New))
		val (_, p2) = tokens.head
		val cname = ClassName(reduce(), p2)
		val args = argsList
		New(cname, args, p1)
	}
	private def reduceWhile(): While ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.While))
		val cond = stmt()
		reduce(Word(Keywords.Do))
		val body = stmt()
		While(cond, body, pos)
	}
	private def reduceIf(): Condition ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.If))
		val cond = stmt()
		reduce(Word(Keywords.Then))
		val tbranch = stmt()
		reduce(Word(Keywords.Else))
		val fbranch = stmt()
		Condition(cond, tbranch, fbranch, pos)
	}
	private def reduceVar(): Assign ={
		val (_, p1) = tokens.head
		reduce(Word(Keywords.Var))
		val name = Var(reduce(), reduceType, p1, true)
		reduce(Operator(Operators.Assign))
		val (_, p2) = tokens.head
		val value = stmt()
		Assign(name, value, p2)
	}
	private def reducePrint(): Print = {
		val (_, p1) = tokens.head
		reduce(Word(Keywords.Print))
		Print(stmt, p1)
	}
	private def reduceInvoke(): Invoke ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.Invoke))
		reduce(OpenParan)
		val className = stmt
		reduce(Coma)
		val methodName = stmt
		reduce(Coma)
		val args = argsList2
		reduce(CloseParan)
		Invoke(className, methodName, args, pos)
	}
	
	private def reduceCT(): CT ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.CT))
		reduce(OpenParan)
		val expr1 = stmt
		reduce(Coma)
		val expr2 = stmt
		reduce(CloseParan)
		CT(expr1, expr2, pos)
	}
	private def reduceRT(): RT ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.RT))
		reduce(OpenParan)
		val expr = stmt
		reduce(CloseParan)
		RT(expr, pos)
	}
	private def reduceIsCT(): IsCT ={
		val (_, pos) = tokens.head
		reduce(Word(Keywords.IsCT))
		reduce(OpenParan)
		val expr = stmt
		reduce(CloseParan)
		IsCT(expr, pos)
	}
	
	
	private def reduce[B <: Token](b: B) = {
		val (token, pos) = tokens.head
		tokens = tokens.tail
		token match{
			case `b` => b
			case _ => error(pos)
		}
	}
	
	private def reduce(): String = {
		val (token, pos) = tokens.head
		tokens = tokens.tail
		token match{
			case ID(name) => name
			case _ => error(pos)
		}
	}
	
	private def reduceType(): Types = {
		reduce(Colon)
		reduceType2()
	}
	
	private def reduceType2(): Types = {
		if(test(Word(Keywords.Numb))) {
			reduce(Word(Keywords.Numb))
			INT
		}
		else if(test(Word(Keywords.Bool))) {
			reduce(Word(Keywords.Bool))
			Bool
		}
		else if(test(Word(Keywords.Str))){
			reduce(Word(Keywords.Str))
		 	Str
		}
		else {
			val (token, pos) = tokens.head
			tokens = tokens.tail
			token match{
				case ID(name) => {
					classes.contains(name) match {
						case true => ClassName(name, pos)
						case false => error(pos)
					}
				}
				case _ => error(pos)
			}
		}
	}
	
	private def test(b: Token): Boolean = {
		val (token, _) = tokens.head
		token match {
			case `b` => true
			case _ => false
		}
	}
	
	private def error(pos: Pos) = {
		println(tokens)
		if(pos.isInstanceOf[Position]){
			val p = pos.asInstanceOf[Position]
			throw new Error("in line " + (p.line+1) + ": \n" +
																	lines(p.line) + "\n" +
																	(" " * (p.col+13)) + "^")
		}
		else //This should never happen 
			throw new Error("An error occured during parsing the text")
	}
	
	def start(): Program = {
		parse()		
	}
	
	private def checkAgainstIllegal() = {
		val illegals = tokens.filter(_._1 == Illegal)
		if(illegals != Nil){
			val (_, pos) = illegals.head
			error(pos)
		}
	}
}


