package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Yuyang Zeng	
   * 
   * Partner: <Ziheng Zhu>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case S(s)	=> if(s==" ")0 else try s.toDouble catch {case _: Exception => Double.NaN }
      case B(b) => if(b) 1 else 0
      case Undefined => Double.NaN
    }
  }


  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n.isNaN()|n == 0) false else true
      case S(s) => if(s==" ")false else true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case B(b) =>b.toString
      case N(n)	=>n.toString
      case Undefined => "undefined"

      case _ =>  throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {

    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case Var(x) => lookup(env,x)//variable x is a  type of string

      case Unary(Neg, e1) =>N(-toNumber(eval(env, e1)))
      case Unary(Not, e1) => B(!toBoolean(eToVal(e1)))


      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), v2) => S(toStr(eToVal(e1))+toStr(eToVal(e2)))
        case (v1, S(s2)) => S(toStr(eToVal(e1))+toStr(eToVal(e2)))
        case (v1, v2) =>  N(toNumber(eToVal(e1))+toNumber(eToVal(e2)))
      }
      case Binary(Minus, e1, e2) => N(toNumber(eToVal(e1))-toNumber(eToVal(e2)))
      case Binary(Times, e1, e2) =>  N(toNumber(eToVal(e1))*toNumber(eToVal(e2)))
      case Binary(Div, e1, e2) =>  N(toNumber(eToVal(e1))/toNumber(eToVal(e2)))

      case Binary(Eq, e1, e2) => B(eToVal(e1) == eToVal(e2))
      case Binary(Ne, e1, e2) => B(eToVal(e1) != eToVal(e2))
      case Binary(Lt, e1, e2) => B(toNumber(eToVal(e1)) < toNumber(eToVal(e2)))
      case Binary(Le, e1, e2) => B(toNumber(eToVal(e1)) <= toNumber(eToVal(e2)))
      case Binary(Gt, e1, e2) => B(toNumber(eToVal(e1)) > toNumber(eToVal(e2)))
      case Binary(Ge, e1, e2) => B(toNumber(eToVal(e1)) >= toNumber(eToVal(e2)))


      case Binary(And, e1, e2) =>
        if(toBoolean(eToVal(e1)))
          eToVal(e2) else eToVal(e1)
      case Binary(Or, e1, e2) =>
        if(toBoolean(e1))
          eToVal(e1) else eToVal(e2)

      case Binary(Seq, e1, e2) => eToVal(e1);eToVal(e2)

      case If(e1, e2, e3) => if(toBoolean(eToVal(e1)))
        eToVal(e2) else eToVal(e3)

      case ConstDecl(x, e1, e2) => eval(extend(env,x,eToVal(e1)), e2)//?????????

      case Print(e1) => println(pretty(eToVal(e1))); Undefined

      case _ => throw new UnsupportedOperationException
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }

}
