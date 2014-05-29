package com.asci

import com.asci.env.Env._
import com.asci.env.Env
import scalaz._
import Scalaz._

object Main extends App {
  lazy val schemeParser = new Parser

  def repl(e: Env): Unit = {
    print("λ: ")
    for (command <- io.Source.stdin.getLines()) {
      schemeParser.read(command) match {
        case schemeParser.Success(foo :: _, _) =>
          import com.asci.Eval._
          import com.asci.Expr.ShowExpr

          foo.eval(e) match {
            case \/-((env, result)) =>
              result.println
              repl(env)
            case -\/(err) =>
              import com.asci.EvalError._
              err.println
              repl(e)
          }
        case schemeParser.NoSuccess(err, _) => println(err)
      }
    }
  }

  println("---------------------------------------------------------")
  println("   The Glorious Another Scheme Console Interpreter")
  println("                      Version 0.1")
  println("---------------------------------------------------------")
  println()
  repl(r5rsEnv)
}
