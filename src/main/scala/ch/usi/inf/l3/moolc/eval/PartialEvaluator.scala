// /*
// * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
//  * All rights reserved.
//  * */
//  
// 
// package ch.usi.inf.l3.moolc.evaluator
// 
// import _root_.ch.usi.inf.l3.moolc.ast._
//  
//  
// class PartialEvaluator(pgm: Program) {
//   private var staticMethodID = 0
//   private var doCheckStore = true
//    def start = {
//     val clazzes = for(clazz <- pgm.classes) yield {
//       val store = new Store
//       for(v <- clazz.body.vars) store.add(v, Bottom)
//       val constStore = store.cloneStore
//       for(v <- clazz.args) constStore.add(v, Bottom)
//       val const = peval(clazz.body.const, constStore)._1
//       for(v <- clazz.body.vars) {
//         store.add(v, constStore.get(v).get)
//       }
//       
//       val methods = for(method <- clazz.body.methods) yield {
//         val mstore = store.cloneStore
//         for(v <- method.args) mstore.add(v, Bottom)
//         val mbody = peval(method.expr, mstore)._1
//         MMethod(method.mod, method.name, method.tpe, method.args,
//               method.pos, mbody)
//       }
//       val smethods = methodBankMap.get(clazz.name.name) match{
//         case Some(bank) => bank.getSpecializedMethodsList
//         case _ => Nil
//       }
//       MClass(clazz.name, clazz.args, 
//         MClassBody(clazz.body.vars, const, smethods ::: methods), clazz.pos)
//     }
//     Program(clazzes ::: classBank.getAllSpecializedClasses)
//   }
//   private def getNextStaticMethod(obj: String, method: String) = {
//     val newName = obj + "_" + method + "_" + staticMethodID
//     staticMethodID += 1
//     newName
//   }
//   private val classBank = new ClassBank
//   private var methodBankMap: Map[String, MethodBank] = Map.empty
//   
//   private def getRuntimeParams(params: List[Var], values: List[PEValue]): 
//      List[Var]= {
//     var temp: List[Var] = Nil
//     val pvTuple = params zip values
//     for((p, v) <- pvTuple;
//       if(! v.isInstanceOf[CTValue])) temp = p :: temp
//     temp.reverse
//   }
//   
//   private def getRuntimeArgs(exprs: List[Expression], values: List[PEValue]):
//      List[Expression]= {
//     var temp: List[Expression] = Nil
//     val pvTuple = values zip exprs
//     for((v, e) <- pvTuple){
//       v match{
//         case CTValue(x) => 
//         case Top | Bottom => temp = e :: temp
//         case AbsValue(x) => temp = x :: temp
//       }
//     }
//     temp.reverse
//   }
//   
//   private def specializeMethod(newName: String, cname: ClassName, mthd: String, 
//               args: List[PEValue], store: StoreTrait) : MMethod = {
//       val clazz = pgm.getClass(cname).get
//       val method = clazz.getMethod(mthd).get
//       val nenv = args zip method.args
//       val newStore = store.cloneStore
//       newStore.addEnv(Map(nenv.map(_.swap): _*))
//       val (expr, _) = peval(method.expr, newStore)
//       val methodArgs = getRuntimeParams(method.args, args)
//       val sMethod = MMethod(method.mod, newName, method.tpe,
//                            methodArgs, NoPosition, expr)
//       sMethod
//   }
//   private def getSpecializedMethod(cname: ClassName, method: String,
//                 args: List[PEValue], store: StoreTrait) : MMethod = {
//     methodBankMap.get(cname.name) match {
//       case None =>
//         val bank = new MethodBank
//         val newName = bank.getMethodName(method)
//         methodBankMap = methodBankMap + (cname.name -> bank)
//         val specializedMethod = specializeMethod(newName, cname, 
//                                                       method, args, store)
//         bank.add(method, args, specializedMethod)
//         //methodBankMap = methodBankMap + (cname.name -> bank)
//         specializedMethod
//       case Some(bank) =>
//         bank.getOption(method, args) match{
//           case Some(x) => x
//           case _ => 
//             val newName = bank.getMethodName(method)
//             val specializedMethod = specializeMethod(newName, 
//                                   cname, method, args, store)
//             bank.add(method, args, specializedMethod)
//             methodBankMap = methodBankMap + (cname.name -> bank)
//             specializedMethod
//         }  
//     }
//   }
//   private def getSpecializedClass(cname: ClassName, 
//                     args: List[PEValue]) : MClass = {
//     classBank.getOption(cname, args) match{
//       case Some(x) => x
//       case None => 
//         val clazz = pgm.getClass(cname).get
//         val clazzArgs = getRuntimeParams(clazz.args, args)
//         val clazzStore = new Store
//         for(v <- clazz.body.vars) clazzStore.add(v, Bottom)
//         val nenv = args zip clazz.args
//         val newStore = clazzStore.newStore(Map(nenv.map(_.swap): _*))
//         val constStore = clazzStore.cloneStore;
//         constStore.addEnv(Map(nenv.map(_.swap): _*))
//         val (const, _) = peval(clazz.body.const, constStore)
//         val methods = specializeMethods(clazz.body.methods, newStore)
//         val sclazz = MClass(classBank.getClassName(cname), clazzArgs, 
//               MClassBody(clazz.body.vars, const, methods), NoPosition)
//         classBank.add(cname, args, sclazz)
//         sclazz
//     }
//   }
//   
//   private def specializeMethods(methods: List[MMethod], store: StoreTrait) = {
//     for(method <- methods) yield {
//       val expr = peval(method.expr, store.cloneStore)._1
//       MMethod(method.mod, method.name, method.tpe, method.args, method.pos,
//           expr)
//     }
//   }
//   private def fevalArgs(args: List[Expression], store: StoreTrait) :
//                                         List[CTValue] = {
//     val evalArgs = for(arg <- args) yield CTValue(feval(arg, store))
//     evalArgs
//   }
//   
//   private def pevalArgs(args: List[Expression], store: StoreTrait) :
//                                           List[PEValue] = {
//     val evalArgs = for(arg <- args) yield peval(arg, store)._2
//     evalArgs
//   }
//   
//   private def pevalArgs2(args: List[Expression], store: StoreTrait) :
//                                           List[Expression] = {
//     val evalArgs = for(arg <- args) yield peval(arg, store)._1
//     evalArgs
//   }
//   
//   private def allCT(list: List[PEValue]) = {
//     if(list == Nil) false
//     else{
//       var test = true
//       var temp = list
//       while(temp != Nil && test){
//         temp.head match{
//           case CTValue(x) => temp = temp.tail
//           case _ => test = false
//         }
//       }
//       test
//     }
//   }
//   private def hasCT(list: List[PEValue]) = {
//     var test = false
//     var temp = list
//     while(temp != Nil && !test){
//       temp.head match{
//         case CTValue(x) => test = true
//         case _ => temp = temp.tail
//       }
//     }
//     test
//   }
//   
//   
//   private def feval(expr: Expression, store: StoreTrait) :
//       Expression = {
//     expr match{
//       case x : Constant => x
//       case obj: ObjectValue => obj
//       case cname: ClassName => cname
//       case Binary(op, x, y, _) => 
//         val x1 = feval(x, store)
//         val y1 = feval(y, store)
//         (x1, y1) match{
//           case (Constant(a, _), Constant(b, _)) => doOp(op, a, b)
//           case _ => Binary(op, x1, y1, NoPosition)
//         }
//       case x: Var => 
//         store.get(x) match{
//           case Some(v: PEKnownValue) => v.getExpr
//           case Some(Top) => throw new Error("You cannot put CT here")
//           case _ => x
//         }
//       case Assign(lhs, rhs, _) =>
//         val r = feval(rhs, store)
//         store.add(lhs, CTValue(r))
//         r
//       case Seq(e1, e2, _) =>
//         feval(e1, store)
//         feval(e2, store)
//       case cond @ Condition(c, t, f, _) =>
//         feval(c, store) match {
//           case (Constant(MBool(true), _)) => feval(t, store)
//           case (Constant(MBool(false), _)) => feval(f, store)
//           case _ => throw new Error(c + " You should not reach here")
//         }
//       case While(c, b, _) =>
//         feval(Condition(c, Seq(b, While(c, b, NoPosition), 
//                             NoPosition), Empty, NoPosition), store)
//       case StaticCall(clazz, mthd, argList, _) => 
//         val args = fevalArgs(argList, store)
//         val method = pgm.getClass(clazz).get.getMethod(mthd).get
//         val nenv = args zip method.args
//         val newStore = store.newStore(Map(nenv.map(_.swap): _*))
//         feval(method.expr, newStore)
//       case DynamicCall(obj, mthd, argList, _) => 
//         store.get(obj) match{
//           case Some(CTValue(ObjectValue(mobj))) =>
//             val args = fevalArgs(argList, store)
//             val method = pgm.getClass(mobj.clazz).get.getMethod(mthd).get
//             val nenv = args zip method.args
//             mobj.store.addEnv(Map(nenv.map(_.swap): _*))
//             feval(method.expr, mobj.store)  
//           case _ => throw new Error("Object values does not exist")
//         }
//       case This(clazz, mthd, argList, _) => 
//         val args = fevalArgs(argList, store)
//         val method = pgm.getClass(clazz).get.getMethod(mthd).get
//         val nenv = args zip method.args
//         val newStore = store.newStore(Map(nenv.map(_.swap): _*))
//         feval(method.expr, newStore)
//       case invoke @ Invoke(cname, mname, args, _) => 
//         val c = feval(cname, store)
//         val m = feval(mname, store)
//         (c, m) match{
//           case (Constant(x: MString, _), Constant(y: MString, _)) =>
//             feval(StaticCall(ClassName(x.value, NoPosition), y.value,
//                         args, NoPosition), store)
//           case _ => invoke
//         }
//       case New(cname, argList, _) => 
//         val args = fevalArgs(argList, store)
//         val clazz = pgm.getClass(cname).get
//         val nenv = args zip clazz.args
//         val newStore = store.newStore(Map(nenv.map(_.swap): _*))
//         val newConst = feval(clazz.body.const, newStore)
//         ObjectValue(MObject(cname, newStore))
//       case Return(expr, _) => feval(expr, store)
//       case Empty | Semi => Empty
//       case _ => throw new Error(expr + "  Print, CT, RT, and IsCT cannot appear here")
//     }
//   }
//   /**
//    * The result of PE is: (expression accompanied with a value, p, which maybe 
//    * compile-time value or an abstraction with a runtime value (or T))
//    * The expression represents the residual code.
//    * The value is the information about the partially evaluated expression.
//    * We follow the following semantics:
//    * 1- PE of primitive value constants always produces abstarct values.
//    * 2- Binary operators, e1 op e2, return a compile-time value if either e1 or 
//    * e2 partially evaluate to a compile-time value, otherwise return an abstract
//    * value.
//    * 3- A variable is compile-time if it is assigned a compile-time value and it
//    * is runtime if it is a T or an approximate value, ~v.
//    * 4- For variables, PE returns their value as the residual expression if they
//    * are compile-time. Otherwise, it returns a residual code which contains the
//    * name of the variable along with the abstract value stored for that variable.
//    * 5- A variable declaration ``var x = e;'', may introduce a compile-time or 
//    * runtime variable, depending on e. If it evaluated to a compile-time variable
//    * then it will have no existence during runtime.
//    * 6- PE of a variable assignment (x = e) depends on whether the x is a compile-time
//    * or runtime variable. For a compile-time variable x, the expression e must evaluate
//    * to a value and the value of x is updated in the store.
//    * 7- When a varaible is assigned to ⟂ that means it has not been yet initialized,
//    * and we can assign it to anything. But when it is bound to T this means we have
//    * no compile time information of this value, therefore we can never bind it with
//    * anything other thing except T.
//    */
//   private def peval(expr: Expression, store: StoreTrait) :
//                        (Expression, PEValue) = {
//     expr match {
//       case x @ Constant(_, _) => (x, AbsValue(x))
//       case binary @ Binary(op, e1, e2, _) => 
//         val (v1, p1) = peval(e1, store)
//         val (v2, p2) = peval(e2, store)
//         (p1, p2) match{
//           case (a: CTValue, b: CTValue) => 
//             val pr = doOp(op, a, b)
//             (pr, CTValue(pr))
//           case (a: CTValue, b: AbsValue) =>
//             val pr = doOp(op, a, b)
//             (pr, CTValue(pr))
//           case (a: AbsValue, b: CTValue) =>
//             val pr = doOp(op, a, b)
//             (pr, CTValue(pr))
//           case (a: AbsValue, b: AbsValue) =>
//             val pr = doOp(op, a, b)
//             (pr, AbsValue(pr))
//           case _ => 
//             (Binary(op, v1, v2, NoPosition), Top)
//         }
//       case x: Var =>
//         store.get(x) match{
//           case p @ Some(CTValue(v)) => (v, p.get)
//           case p @ Some(_) => (x, p.get)
//           case None => 
//             store.add(x, Bottom)
//             (x, Bottom)
//         }
//       case assign @ Assign(x, e, _) =>
//         store.get(x) match {
//           case Some(CTValue(v)) => 
//             // compile-time variables not residualized
//             if(!isCT(e, store))
//               throw new Error(e + " should be a compile-time value")
//             val vprime = feval(e, store)
//             store.add(x, CTValue(vprime))
//             (vprime, CTValue(vprime))
//           case Some(AbsValue(v)) =>
//             // abstract variables must stay abstract
//             val (eprime, p1) = peval(e, store)
//             store.add(x, p1)
//             (Assign(x, eprime, NoPosition), p1)
//           case Some(Top) =>
//             // unknown runtime value
//             val (eprime, p1) = peval(e, store)
//             (Assign(x, eprime, NoPosition), Top)
//           case Some(Bottom) =>
//             // uninitialized variable
//             val (eprime, p1) = peval(e, store)
//             store.add(x, p1)
//             // first assignment determines status of variable
//             p1 match{
//               case CTValue(v) => (Assign(x, v, NoPosition), p1)
//               case _ => (Assign(x, eprime, NoPosition), Top)
//             }
//           case None => 
//             // uninitialized variable
//             val (eprime, p1) = peval(e, store)
//             store.add(x, p1)
//             // first assignment determines status of variable
//             p1 match{
//               case CTValue(v) => (Assign(x, v, NoPosition), p1)
//               case _ => (Assign(x, eprime, NoPosition), Top)
//             }
//         }
//       case Seq(e1, e2, _) =>
//         val (e1prime, _) = peval(e1, store)
//         val (e2prime, p2) = peval(e2, store)
//         (Seq(e1prime, e2prime, NoPosition), p2)
//       case Condition(c, t, f, _) =>
//         val (cprime, p1) = peval(c, store)
//         p1 match{
//           case CTValue(Constant(MBool(true), _)) => peval(t, store)
//           case CTValue(Constant(MBool(false), _)) => peval(f, store)
//           case _ => 
//             val r = checkStore(store, Condition(cprime, t, f, NoPosition), t, f)
//             doCheckStore = true
//             r
//         }
//       case loop @ While(c, body, _) =>
//         val (cprime, p1) = peval(c, store)
//         p1 match{
//           case CTValue(Constant(MBool(true), _)) => peval(Seq(body, 
//                               While(c, body, NoPosition), NoPosition), store)
//           case CTValue(Constant(MBool(false), _)) => peval(Empty, store)
//           case _ => 
//             val r = checkStore(store, loop, c, body)
//             doCheckStore = true
//             r
//         }
//       case init @ New(c, argList, _) => 
//         val args = pevalArgs(argList, store)
//         if(args!= Nil && hasCT(args)){
//           val specializedClazz = getSpecializedClass(c, args)
//           val newArgs = getRuntimeArgs(argList, args)
//           val mobj = ObjectValue(MObject(specializedClazz.name, new Store))
//           (New(specializedClazz.name, newArgs, NoPosition), AbsValue(mobj))
//         }
//         else{
//           val clazz = pgm.getClass(c).get
//           val objStore = new Store
//           for(v <- clazz.body.vars) objStore.add(v, Top)
//           val mobj = ObjectValue(MObject(clazz.name, objStore))
//           (init, AbsValue(mobj))
//         }
//       case This(clazz, mthd, argList, _) => 
//         val method = pgm.getClass(clazz).get.getMethod(mthd).get
//         val args = pevalArgs(argList, store)
//         if(allCT(args)){
//           val nenv = args zip method.args
//           val newStore = new Store
//           newStore.addEnv(Map(nenv.map(_.swap): _*))
//           val result = feval(method.expr, newStore)
//           (result, CTValue(result))
//         }
//         else if(hasCT(args)){
//           val runtimeArgs = getRuntimeArgs(argList, args)
//           val nenv = args zip method.args
//           val newStore = new Store
//           newStore.addEnv(Map(nenv.map(_.swap): _*))
//           val newMethod = getSpecializedMethod(clazz, mthd, args, newStore)
//           (This(clazz, newMethod.name, runtimeArgs, NoPosition), 
//                     Top)
//         }
//         else
//           (This(clazz, mthd, getRuntimeArgs(argList, args), 
//                               NoPosition), Top)
//       case Invoke(ec, em, argList, _) =>
//         val (ecAux, _) = peval(ec, store)
//         val (emAux, _) = peval(em, store)
//         (ecAux, emAux) match{
//           case (Constant(MString(clazz), _), Constant(MString(method), _)) =>
//             peval(StaticCall(ClassName(clazz, NoPosition), method, argList,
//                                   NoPosition), store)
//           case _ =>
//             val args = pevalArgs2(argList, store)
//             val newInvoke = Invoke(ecAux, emAux, args, NoPosition)
//             (newInvoke, Top)
//         }
//       case StaticCall(clazz, mthd, argList, _) =>
//         val args = pevalArgs(argList, store)
//         val method = pgm.getClass(clazz).get.getMethod(mthd).get
//         if(allCT(args)){
//           val nenv = args zip method.args
//           val newStore = new Store
//           newStore.addEnv(Map(nenv.map(_.swap): _*))
//           val result = feval(method.expr, newStore)
//           (result, CTValue(result))
//         }
//         else if(hasCT(args)){
//           val runtimeArgs = getRuntimeArgs(argList, args)
//           val nenv = args zip method.args
//           val newStore = new Store
//           newStore.addEnv(Map(nenv.map(_.swap): _*))
//           val newMethod = getSpecializedMethod(clazz, mthd, args, newStore)
//           (StaticCall(clazz, newMethod.name, runtimeArgs, NoPosition), 
//                     Top)
//         }
//         else
//           (StaticCall(clazz, mthd, getRuntimeArgs(argList, args), NoPosition)
//                 , Top)
//       case DynamicCall(obj, mthd, argList, _) =>
//         val args = pevalArgs(argList, store)
//         val (clazz, method) = obj.tpe match{
//           case x: ClassName => 
//             (x, pgm.getClass(x).get.getMethod(mthd).get)
//           case _ => throw new Error("I should never be reached, how this happened?")
//         }
//         store.get(obj) match{
//           case Some(CTValue(ObjectValue(obv))) =>
//             if(allCT(args)){
//               val nenv = args zip method.args
//               obv.store.addEnv(Map(nenv.map(_.swap): _*))
//               val result = feval(method.expr, obv.store)
//               (result, CTValue(result))
//             }
//             else{
//               val runtimeArgs = getRuntimeArgs(argList, args)
//               val runtimeParams = getRuntimeParams(method.args, args)
//               val newStore = obv.store.cloneStore
//               val nenv = args zip method.args
//               newStore.addEnv(Map(nenv.map(_.swap): _*))
//               val (expr, _) = peval(method.expr, newStore)
//               val mclazz = pgm.getClass(clazz).get
//               val methodName = getNextStaticMethod(obj.name, mthd)
//               val newMethod = MMethod(ClassMod, methodName, method.tpe,
//                       runtimeParams, NoPosition, expr)
//               mclazz.body.methods = newMethod :: mclazz.body.methods
//               (StaticCall(clazz, methodName, runtimeArgs, NoPosition), Top)
//             }
//           case Some(AbsValue(ObjectValue(obv))) => 
//             if(hasCT(args)){
//               val runtimeArgs = getRuntimeArgs(argList, args)
//               val nenv = args zip method.args
//               val newStore = obv.store
//               newStore.addEnv(Map(nenv.map(_.swap): _*))
//               val newMethod = getSpecializedMethod(clazz, mthd, args, newStore)
//               (DynamicCall(obj, newMethod.name, runtimeArgs, NoPosition), 
//                         Top)
//             }
//             else{
//               (DynamicCall(obj, mthd, getRuntimeArgs(argList, args), 
//                                 NoPosition), Top)
//             }
//           case _ => // target is unknown
//             if(hasCT(args)){
//               val runtimeArgs = getRuntimeArgs(argList, args)
//               val nenv = args zip method.args
//               val newStore = new Store
//               newStore.addEnv(Map(nenv.map(_.swap): _*))
//               val newMethod = getSpecializedMethod(clazz, mthd, args, newStore)
//               (DynamicCall(obj, newMethod.name, runtimeArgs, NoPosition), 
//                         Top)
//             }
//             else{
//               (DynamicCall(obj, mthd, getRuntimeArgs(argList, args), 
//                                 NoPosition), Top)
//             }
//         }
//       case CT(e1, e2, _) => 
//         //These two tests should be moved to the feval, I think
//         if(!isCT(e1, store)) 
//           throw new Error(e1 + " should be a compile-time value")
//         if(!isCT(e2, store)) 
//           throw new Error(e2 + " should be a compile-time value")
//         
//         val v2 = feval(e2, store)
//         v2 match{
//           case Constant(MBool(true), _) => 
//             val v1 = feval(e1, store)
//             (v1, CTValue(v1))
//           case _ => peval(e1, store)
//         }
//       
//       case IsCT(e1, _) =>
//         val (v1, p1) = peval(e1, store)
//         p1 match{
//           case CTValue(_) =>
//             val con = Constant(MBool(true), NoPosition)
//             (con, CTValue(con))
//           case _ => 
//             val con = Constant(MBool(false), NoPosition)
//             (con, CTValue(con))
//         }
//       case RT(e1, _) =>
//         val (v1, _) = peval(e1, store)
//         (v1, Top)
//       case Print(x, _) => 
//         val (newX, p1) = peval(x, store)
//         (Print(newX, NoPosition), p1)
//       case Return(x, _) =>
//         val (newX, p1) = peval(x, store)
//         (Return(newX, NoPosition), p1)
//       case Empty | Semi | ClassName(_, _) | ObjectValue(_) => (Empty, Bottom)
//       // case CT(expr, test, _) => 
//       //         peval(test, store, true) match {
//       //           case (Constant(MBool(x), _), _) => peval(expr, store, x)
//       //           case _ => throw new Error(test + " should be a compiler time value,"+
//       //                                          " and it should be bound to boolean")
//       //         }
//       //       case RT(expr, _) => (expr, store)
//       //       case IsCT(expr, _) => (Constant(MBool(isCT(expr, store)), NoPosition), store)
//     }
//   }
//   
//   
//   private def doOp(op: Operation, s1: PEKnownValue, s2: PEKnownValue): Expression ={
//     (s1.getExpr, s2.getExpr) match{
//       case (v1: Constant, v2: Constant) =>
//         val x = v1.value
//         val y = v2.value
//         doOp(op, x, y)
//       case _ => Binary(op, s1.getExpr, s2.getExpr, NoPosition)
//     }
//   }
//   
//   
//   private def doOp(op: Operation, x: Premitive, y: Premitive): Constant = {
//     val result = op match {
//       case Add =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => 
//             MInt(a + b)
//           case (MString(_), MInt(_))
//              | (MInt(_), MString(_)) 
//              | (MString(_), MString(_)) => 
//             MString("" + x.optionValue.get + y.optionValue.get)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Sub =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MInt(a - b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Mul =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MInt(a * b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Div =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MInt(a / b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Eq => MBool(x.optionValue.get == y.optionValue.get)
//       case Neq => MBool(x.optionValue.get != y.optionValue.get)
//       case Lt =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MBool(a < b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Gt =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MBool(a > b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//       case Mod =>
//         (x, y) match{
//           case (MInt(a), MInt(b)) => MInt(a % b)
//           case _ => throw new Error("" + x + y +" Never happens")
//         }
//     }
//     Constant(result, NoPosition)
//   }
//   
//   /**
//    * Implement this method like that:
//    * CT op _ = CT
//    */
//   private def isCT(expr: Expression, store: StoreTrait) = {
//     true
//   }
//   
//   /**
//    * According to the HPE paper I should throw an error if the branches changed
//    * inconsistently, but I prefer to remove the differences and continue
//    * compilation
//    */
//   private def checkStore(store: StoreTrait, c: Expression, e2: Expression,
//                       e3: Expression) = {
//     if(doCheckStore){
//       doCheckStore = false
//       val thenStore = store.cloneStore
//       val (e2prime, p2) = peval(e2, thenStore)
//       val elseStore = store.cloneStore
//       val (e3prime, p3) = peval(e3, elseStore)
//       (store, thenStore, elseStore) match{
//         case (o: Store, t: Store, e: Store) => o.makeConsistent(t, e)
//         case _ => throw new Error("This class only deals with Store, not the"+
//                 "other variants of store")
//       }
//       c match{
//         case Condition(cond, t, f, _) =>
//           (Condition(cond, e2prime, e3prime, NoPosition), Bottom)
//         case While(cond, body, _) =>
//           (While(e2prime, e3prime, NoPosition), Bottom)
//         case _ => throw new Error("You should either pass while or for to here")
//       }
//     }
//     else{
//       c match{
//         case Condition(cond, t, f, _) =>
//           (Condition(cond, e2, e3, NoPosition), Bottom)
//         case While(cond, body, _) =>
//           (While(e2, e3, NoPosition), Bottom)
//         case _ => throw new Error("You should either pass while or for to here")
//       }
//     }
//   }
// }