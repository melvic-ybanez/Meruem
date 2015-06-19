/**
 * Created by ybamelcash on 4/27/2015.
 */

import meruem.Constants._
import meruem._
import meruem.builtins._

import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    Globals.environment += (ModuleSymbol, Globals.module)
    
    args match {
      case Array() => 
        val importExpr = LispList(LispString(Globals.preloadedString))
        Import(importExpr)(Globals.environment) match {
          case error: LispError => throw new InstantiationException(error.toString)
          case SomeModule(_, _, env) => repl(env)
        }
      case Array(mainFile) =>Import(LispString(mainFile) !: NilLispList)(Globals.environment) match {
          case error: LispError => throw new InstantiationException(error.value)
          case SomeModule(_, _, env) => env.get(MainSymbol) match {
            case error: LispError => throw new InstantiationException(error.value)
            case lval => Evaluate(lval)(env)
          }
        } 
      case Array(_, _, _*) => throw new InstantiationException("Invalid number of arguments")
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
