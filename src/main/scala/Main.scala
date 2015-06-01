/**
 * Created by ybamelcash on 4/27/2015.
 */

import scala.collection.JavaConverters._

import meruem.Constants._
import meruem._

import io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val globalEnv = Globals.environment + 
      (LispSymbol("main-args"), LispString(args.foldLeft("")(_ + _).mkString(" ")))
    val environment = Settings.preloads.asScala.foldLeft[Environment](globalEnv) { (environment, module) =>
      // Create the import string. e.g '(import "prelude.me")'
      val importExpr = s"""$OpenParen${Keywords.Import} "$module"$CloseParen"""
      
      Utils.evalExpression(importExpr, environment) match {
        case error: LispError => throw new InstantiationException(error.toString)
        case SomeModule(filePath, _, newEnvironment) => newEnvironment
      }
    }
    repl(environment)
  }

  // READ-EVAL-PRINT-LOOP
  def repl(environment: Environment) {
    val input = readLine("meruem> ")
    Utils.evalExpression(input, environment) match {
      case ldef @ LispDef(newEnvironment) =>
        println(ldef)
        repl(newEnvironment)
      case lval =>
        println(lval)
        repl(environment)
    }
  }
}
