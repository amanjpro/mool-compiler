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

package ch.usi.inf.l3.moolc

import lexer._
import ast._
import semantics._
import parser._

class Compile(file: String){
	def start: Unit = {
		val lexer = new Lexer(file)
		lexer.start
		val parser = new Parser(lexer.lines, lexer.getTokens)
		val ast = parser.start
		new SemanticsAnalyzer(ast, lexer.lines).start
	}
}
object Compile {
	
	def main(args: Array[String]) = {
		if(args.length != 1 && args(0).endsWith("mool")) 
			println("Usage: Parser filname.mool")
		new Compile(args(0)).start
	}
}