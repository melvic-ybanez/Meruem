/**
 * Created by ybamelcash on 4/27/2015.
 */

import scala.collection.JavaConverters._

import meruem.Constants._
import meruem.Environment._
import meruem._

import io.StdIn.readLine
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val importExprs = Settings.preloads.asScala.map { module =>
      // Create the import string. e.g '(import "prelude.me")'
      s"""$OpenParen${Keywords.Import} "$module"$CloseParen"""
    }.mkString

    val environment = SomeEnvironment(mutable.Map(Keywords.Module -> Globals.module), Globals.environment)
    
    Utils.evalExpression(importExprs, environment) match {
      case error: LispError => throw new InstantiationException(error.toString)
      case module: Module => repl(environment)
    }
  } 

  // READ-EVAL-PRINT-LOOP
  def repl(env: Environment): Unit = readLine("meruem> ") match {
    case Settings.exitCommand => 
      println("Bye!")
      sys.exit(0)
    case input =>
      println(Utils.evalExpression(input, env))
      repl(env)
  }
}
