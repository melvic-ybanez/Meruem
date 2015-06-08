package meruem

import java.nio.file.{Paths, Path}

import scala.util.parsing.combinator.Parsers

import meruem.Constants._
import meruem.Implicits._
import meruem.LispParser._

import scala.util.parsing.input.{NoPosition, CharSequenceReader, CharArrayReader}

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  def read[A <: LispValue](expression: LispParser.Parser[A], input: String)
                          (f: A => LispValue)
                          (implicit env: Environment): LispValue = {
    LispParser.parseAll(expression, new CharArrayReader(input.toArray)) match {
      case Success(expr, _) => f(expr)
      case failure@Failure(msg, next) =>  Errors.parseFailure(msg, next.pos) 
      case error@Error(msg, next) => Errors.parseError(msg, next.pos)
    }
  }
  
  def evalExpression(str: String, environment: Environment): LispValue = 
    read(expression, str)(Evaluate(_)(environment))(environment)

  def macroExpand(lval: LispValue)(implicit env: Environment): LispValue = lval match {
    case LispDefMacro(func) !: tail =>
      macroExpand(Evaluate(func.updated(args = tail.map(x => LispList(LispQuoteSymbol, x)))))
    case _ => lval
  }
  
  def whenValid[A <: LispValue, B <: LispValue](lval: A)(f: A => B) = lval match {
    case error: LispError => error
    case lval => f(lval)
  }

  def checkArgsCount(args: LispList)(p: Int => Boolean)(f: => LispValue)(implicit env: Environment) =
    if (!p(args.size)) Errors.incorrectArgCount(args.size, args) else f

  def sanitizeAll(args: LispList)(f: (LispList, Environment) => LispValue)(implicit env: Environment) =  
    whenValid(args.foldLeft[LispValue](NilLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(Evaluate(lval))(_ !: llist)
      }
    }) {
      case lval: LispList => f(lval.reverse, env)
      case error: LispError => error
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  
  def withCollArg(args: LispList)(f: LispList => LispValue)(g: LispString => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ == 1) {
      args.head match {
        case llist: LispList => f(llist)
        case str: LispString => g(str)
        case lval => Errors.invalidType(LispTypeStrings.List, lval)
      }
    }
  
  def isPair(expr: LispValue) = expr match {
    case ConsLispList(_, ConsLispList(_, NilLispList)) => true
    case _ => false
  }
  
  def withPairListArgs(args: LispList)(f: => LispValue)(implicit env: Environment) = checkArgsCount(args)(_ > 0) {
    args.find(!isPair(_)).map(expr => Errors.invalidType(LispTypeStrings.Pair, expr)) getOrElse f 
  }
  
  def allSymbols(llist: LispList)(f: => LispValue)(implicit env: Environment) = llist.find {
    case _: LispSymbol => false
    case _ => true
  } map(lval => Errors.invalidType(LispTypeStrings.Symbol, lval)) getOrElse f
  
  def withStringArg(args: LispList)(f: String => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ == 1) {
      whenValid(Evaluate(args.head)) {
        case LispString(str) => f(str)
        case lval => Errors.invalidType(LispTypeStrings.String, lval)
      }
    }
  
  def whenNumber[A](lval: LispValue)
                    (f: Int => A)
                    (g: Long => A)
                    (h: Float => A)
                    (k: Double => A)
                    (implicit env: Environment): LispValue = lval match {
    case LispInt(x) => f(x)
    case LispLong(x) => g(x)
    case LispFloat(x) => h(x)
    case LispDouble(x) => k(x)
    case _ => Errors.invalidType(LispTypeStrings.Integer, lval)
  }
  
  def withSingleArg(args: LispList)(f: LispValue => LispValue)(implicit env: Environment): LispValue = 
    checkArgsCount(args)(_ == 1)(args match {
      case expr !: _ => f(expr)
    })
  
  def withAtLeastOneArg(args: LispList)(f: LispList => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ > 0)(f(args))

  def compute[A, B, C, D, E, F](lnum1: LispNumber[A])
                               (lnum2: LispNumber[B])
                               (f: (Int, Int) => C)
                               (g: (Long, Long) => D)
                               (h: (Float, Float) => E)
                               (k: (Double, Double) => F): Any = (lnum1.value, lnum2.value) match {
    case (x: Int, y: Int) => f(x, y)
    case (x: Int, y: Long) => g(x, y)
    case (x: Long, y: Int) => g(x, y)
    case (x: Long, y: Long) => g(x, y)
    case (x: Int, y: Float) => h(x, y)
    case (x: Float, y: Int) => h(x, y)
    case (x: Long, y: Float) => h(x, y)
    case (x: Float, y: Long) => h(x, y)
    case (x: Float, y: Float) => h(x, y)
    case (x: Int, y: Double) => k(x, y)
    case (x: Double, y: Int) => k(x, y)
    case (x: Long, y: Double) => k(x, y)
    case (x: Double, y: Long) => k(x, y)
    case (x: Float, y: Double) => k(x, y)
    case (x: Double, y: Float) => k(x, y)
    case (x: Double, y: Double) => k(x, y)
  }
  
  def isDefineCommand(str: String) = List(Keywords.Def, Keywords.Defun, Keywords.DefMacro) contains str
}
