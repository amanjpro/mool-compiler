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

package ch.usi.inf.l3.moolc.lexer

import _root_.ch.usi.inf.l3.moolc.ast._
import io.Source

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


 
 
object Keywords extends Enumeration {
	type Keywords = Value
	val Class, Method, Var, Init, Static, If, Then, Else, While, Do, New,
		Invoke, CT, RT, IsCT, Str, Numb, Bool, Void, Return, Print = Value
}

object Operators extends Enumeration {
	type Operators = Value
	val Add, Sub, Mul, Div, Eq, Neq, Gt, Lt, Mod, Assign = Value
}


sealed abstract class Token {}
case class Operator(op: Operators.Value) extends Token {}
case class Word(tpe: Keywords.Value) extends Token {}
case class ID(name: String) extends Token {}
case class ValInt(value: Int) extends Token {}
case class ValString(value: String) extends Token {}
case class ValBool(value: Boolean) extends Token {}
case object ValNull extends Token {}
case object Semicolon extends Token {}
case object OpenParan extends Token {}
case object CloseParan extends Token {}
case object OpenBrace extends Token {}
case object CloseBrace extends Token {}
case object Coma extends Token {}
case object Illegal extends Token {}
case object Dot extends Token {}
case object Colon extends Token {}


class Lexer(file: String) {
	val lines = read(file)
	//Token, line number
	private var tokens: List[(Token, Pos)] = Nil
	
	def getTokens: List[(Token, Pos)] = tokens
	
	private def lex(pgm: String): Unit = {
		var col = 0
		var token = ""
		var i = 0
		var lnum = 0
		
		while(i < pgm.length){
			var ch = pgm.charAt(i)
			ch match{
				case '_' => {
					i = i + 1
					col = 0
					tokens = (Illegal, Position(col, lnum)) :: tokens
				}
				case '\n' => {
					lnum = lnum + 1
					i = i + 1
					col = 0
				}
				case '.' => 
					tokens = (Dot, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '+' => 
					tokens = (Operator(Operators.Add), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '-' => 
					tokens = (Operator(Operators.Sub), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '*' => 
					tokens = (Operator(Operators.Mul), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '/' => 
					if(pgm.charAt(i+1) == '/'){
						while(i < pgm.length && ch != '\n'){
							i = i + 1
							ch = pgm.charAt(i)
						}
						col = 0
						lnum +=1
					}
					else {
						tokens = (Operator(Operators.Div), Position(col, lnum)) :: tokens
						i = i + 1
						col += 1
					}
				case '=' => {
					if(pgm.charAt(i+1) == '='){
						tokens = (Operator(Operators.Eq), Position(col, lnum)) :: tokens
						i = i + 1
						col += 1
					}
					else tokens = (Operator(Operators.Assign), Position(col, lnum)) :: tokens
					
					i = i + 1
					col += 1
				}
					
				case '!' => {
					if(pgm.charAt(i+1) == '='){
						tokens = (Operator(Operators.Neq), Position(col, lnum)) :: tokens
						i = i + 1
						col += 1
					}
					else (Illegal, Position(col, lnum)) :: tokens
					
					i = i + 1
					col += 1
				}
				case '<' => 
					tokens = (Operator(Operators.Gt), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '>' => 
					tokens = (Operator(Operators.Lt), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '%' => 
					tokens = (Operator(Operators.Mod), Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case ';' => 
					tokens = (Semicolon, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '{' =>
					tokens = (OpenBrace, Position(col, lnum)) :: tokens
					i = i + 1
					col +=1
				case '}' =>
					tokens = (CloseBrace, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '[' | ']' =>
					(Illegal, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case '(' =>
					tokens = (OpenParan, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case ')' =>
					tokens = (CloseParan, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case ',' =>
					tokens = (Coma, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case ':' => 
					tokens = (Colon, Position(col, lnum)) :: tokens
					i = i + 1
					col += 1
				case ' ' | '\t'=>
					i = i + 1
					col += 1
				case '\"' => {
					var word = ""
					i = i + 1
					col += 1
					ch = pgm.charAt(i)
					while(i < pgm.length && ch != '\"'){
						word = word + ch
						i = i + 1
						col += 1
						ch = pgm.charAt(i)
					}
					if(ch == '\"'){
						i = i + 1
						col += 1
						tokens = (ValString(word), Position(col, lnum)) :: tokens
					}
					else tokens = (Illegal, Position(col, lnum)) :: tokens
				}
				case _ => {
					var word = ""
					while(i < pgm.length && !isBreaker(ch)){
						word = word + ch
						i = i + 1
						col += 1
						ch = pgm.charAt(i)
					}
					val lexem = word match {
						case "class" => Word(Keywords.Class)
						case "method" => Word(Keywords.Method)
						case "static" => Word(Keywords.Static)
						case "var" => Word(Keywords.Var)
						case "init" => Word(Keywords.Init)
						case "if" => Word(Keywords.If)
						case "then" => Word(Keywords.Then)
						case "else" => Word(Keywords.Else)
						case "while" => Word(Keywords.While)
						case "do" => Word(Keywords.Do)
						case "new" => Word(Keywords.New)
						case "invoke" => Word(Keywords.Invoke)
						case "CT" => Word(Keywords.CT)
						case "RT" => Word(Keywords.RT)
						case "IsCT" => Word(Keywords.IsCT)
						case "true" => ValBool(true)
						case "false" => ValBool(false)
						case "null" => ValNull
						case "int" => Word(Keywords.Numb)
						case "str" => Word(Keywords.Str)
						case "bool" => Word(Keywords.Bool)
						case "void" => Word(Keywords.Void)
						case "return" => Word(Keywords.Return)
						case "print" => Word(Keywords.Print)
						case _ => {
							try {
								ValInt(word.toInt)
							} catch {
							  case _ : java.lang.NumberFormatException => ID(word)
							}
						}
					} 
					tokens = (lexem, Position(col, lnum)) :: tokens
				}
			}
		}
		tokens = tokens.reverse
	}
	
	private def isBreaker(ch: Char): Boolean = {
		ch match {
			case ' ' | '\n' | '\r' | '+' | '-' | '=' | ';' | '*' | '%' | '/' | '!' 
					| '.' | '{' | '}' | '(' | ')' | '[' | ']' | '\"' | ',' | '\t' | ':'=> 
					true
			case _ => false 
			
		}
	}
	
	private def read(file: String) = {
		val source = Source.fromFile(file)
		val src: List[String] = source.getLines.toList
		source.close
		src.toArray
	}
	def start: Unit = {
		lex(lines.mkString("\n"))
	}
}
