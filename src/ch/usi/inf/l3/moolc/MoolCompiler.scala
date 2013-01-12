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
import ir._
import codegen._
import evaluator._
import java.io._

class Compile(file: String, pe: Boolean){
	private def start: Unit = {
		val lexer = new Lexer(file)
		lexer.start
		val parser = new Parser(lexer.lines, lexer.getTokens)
		val ast = parser.start
		new SymbolTableChecker(ast, lexer.lines).start
		val newPgm = if(pe) {
			val peval = new PartialEvaluator(ast)
			peval.start
		}
		else {
			ast
		}
		val pgm = new IntermediateCode(newPgm).start
		val path = new File(file).getParent
		printToFile(printIR(pgm), file + ".ir")
		new BytecodeGenerator(pgm, path).start
	}
	
	
	private def printToFile(str: String, dest: String) = {
		val fos = new PrintWriter(new FileOutputStream(dest))
		fos.write(str)
		fos.close
	}
	private def printIR(pgm: IRProgram): String = { 
		val classes = for(clazz <- pgm.classes) yield{
			val name = "class " + clazz.name + "{\n"
			val fields = printFields(clazz.vars) + "\n"
			val init = clazz.name + "(" + printParams(clazz.args) + ")"
			val exprs = "{\n" + printExpr(clazz.const) + "\n}\n"
			val methods = for(method <- clazz.methods) yield {
				val mod = method.isStatic match{
					case true => "static "
					case false => ""
				}
				mod + printType(method.tpe) + method.name + 
							"(" + printParams(method.args) + "){\n" + 
							printExpr(method.body) + "\n}"
			}
							
			name + fields + init + exprs + methods.mkString("\n") + "\n}\n"
		}
		classes.mkString("\n")+"\n"
	}
	
	private def printFields(vars: List[IRVar]) : String = {
		val fields = for(v <- vars) yield {
			printType(v.tpe) + v.name + ";"
		}
		fields.mkString("\n")
	}
	private def printParams(params: List[IRVar]) : String = {
		val argsAux = for(arg <- params) yield{
			printType(arg.tpe) + arg.name + ","
		}
		val args = argsAux.mkString(" ")
		try{
			args.substring(0, args.length-1)
			}catch{
				case ex: StringIndexOutOfBoundsException => args
			}
	}
	
	private def printArgs(a: List[IRVar]): String = {
		val argsAux = for(arg <- a) yield{
			arg.name + ","
		}
		val args = argsAux.mkString(" ")
		try{
			args.substring(0, args.length-1)
			}catch{
				case ex: StringIndexOutOfBoundsException => args
			}
	}
	private def printExpr(exprs: List[IRExpr]): String = {
		val r = for(expr <- exprs) yield {
			printExpr(expr)
		}
		r.mkString("\n") + "\n"
	}
	
	private def printExpr(expr: IRExpr): String = {
		expr match{
			case IRBinary(e1, e2, op, e3) => 
				e1.name + " = " + e2.name + printOp(op) + e3.name + ";"
			case IRAssignVar(e, v) => e.name + " = " + v.name + ";"
			case IRAssignConst(e, v) => e.name + " = " + v.value.get + ";"
			case IRAssignCall(e, f) => e.name + " = " + printExpr(f)
			case IRAssignNew(e, n) => e.name + " = " + printExpr(n)
			case IRVar(n, tpe) => printType(tpe) + n + ";"
			case IRNotVar(v) => "not " + v.name + ";"
			case IRLabel(l) => l+":"
			case IRIF(c, g) => "if not " + c.value.name + " goto " + g.l
			case IRWhileIf(c, g) => "if " + c.name + " goto " + g.l
			case IRGoto(l) => "goto " + l.l + ";"
			case IRPrint(v) => "System.out.println(" + v.name + ");"
			case IRCall(c, m, args, _) => c + "." + m + "(" + printArgs(args) + ");"
			case IRInvoke(c, m, args) => "invoke(" + c.name + ", " + 
																			m.name + ", " + printArgs(args) + ");"
			case IRNew(c, args) => "new " + c + "(" + printArgs(args) + ");"
			case IRReturn(v) => "return " + v.name + ";"
			case _ => ""
		}
	}
	
	private def printOp(op: IROp.Value): String = {
		op match{
			case IROp.Add => " + "
			case IROp.Sub => " - "
			case IROp.Mul => " * "
			case IROp.Div => " / "
			case IROp.Eq => " == "
			case IROp.Neq => " != "
			case IROp.Gt => " > "
			case IROp.Lt => " < "
			case IROp.Mod => " % "
			case IROp.Assign => " = "
		}
	}
	private def printType(tpe: IRType) = {
		tpe match {
			case IRInt => "int "
			case IRStr => "String "
			case IRBool => "boolean "
			case IRVoid => "void "
			case IRObject(x) => x + " "
			case _ => throw new Error("Should not happen")
		}
	} 
}
object Compile {
	
	def main(args: Array[String]) = {
		var optimize = false
		if(args.length == 1 && !args(0).endsWith("mool")) {
			println("Usage: ./moolc.sh [options] filname.mool")
			System.exit(1)
		}
		else if(args.length == 2 && (!args(1).endsWith("mool") ||
																	args(0) != "-pe")) {
			println("Usage: ./moolc.sh [options] filname.mool")
			System.exit(1)
		}
		if(args.length == 2) new Compile(args(1), true).start
		else new Compile(args(0), false).start
	}
}