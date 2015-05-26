/**
 * Created by ybamelcash on 4/27/2015.
 */

import scala.collection.JavaConverters._

import meruem.Constants._
import meruem._

import io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val environment = Settings.preloads.asScala.foldLeft[Environment](Globals.environment) { (environment, module) =>
      // Create the import string. e.g '(include "prelude.me")'
      val importExpression = s"""$OpenParen${Keywords.Include} "$module"$CloseParen"""
      
      Utils.evalExpression(importExpression, environment) match {
        case error: LispError => throw new InstantiationException(error.toString)
        case ldef @ LispDef(newEnvironment) => newEnvironment
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
