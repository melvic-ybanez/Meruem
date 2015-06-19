/**
 * Created by ybamelcash on 4/27/2015.
 */

import meruem.Constants._
import meruem._
import meruem.Implicits.listToLispList
import meruem.builtins._

import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    Globals.environment += (ModuleSymbol, Globals.module)
    
    if (args.isEmpty) {
      val importExpr = LispList(LispString(Globals.preloadedString))
      Import(importExpr)(Globals.environment) match {
        case error: LispError => throwError(error.toString)
        case SomeModule(_, _, env) => repl(env)
      }
    } else Import(LispString(args.head) !: NilLispList)(Globals.environment) match {
      case error: LispError => throwError(error.value)
      case SomeModule(_, _, env) => env.get(MainSymbol) match {
        case error: LispError => throwError(error.value)
        case mainFunc: LispLambda => 
          val largs = LispList(QuoteSymbol, args.tail.map(LispString).toList) !: NilLispList
          Evaluate(mainFunc.updated(args = largs))(env) match {
            case error: LispError => throwError(error.value)
            case _ => 
          }
        case lval => throwError("Invalid format for main: " + lval)
      }
    }
  } 

  // READ-EVAL-PRINT-LOOP
  def repl(env: Environment): Unit = readLine("meruem> ") match {
    case Settings.exitCommand => 
      println("Bye!")
      sys.exit(0)
    case input =>
      Utils.evalExpression(input, env) match {
        case LispChar(c) => println("\\" + c)
        case lval => println(lval)
      }
      repl(env)
  }
  
  def throwError(msg: String) = throw new InstantiationException(msg)
}
