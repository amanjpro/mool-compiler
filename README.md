mool-compiler
=============

Mool Compiler, is a toy compiler written in Scala, which compiles programs written in Mool to Java bytecode

To compile the compiler:

1- You need to have JDK, Scala SDK, and Ant
2- Open the terminal and write
	ant jar
	
And to run the mool compiler:
1- Open the terminal and write:
	./mool.sh path/to/the/file.mool
	
The semantics of the language:

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
