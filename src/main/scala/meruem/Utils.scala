package meruem

import scala.util.parsing.combinator.Parsers

import meruem.Constants.{Keywords, LispTypeStrings}
import meruem.Implicits._
import meruem.LispParser._
import scala.util.parsing.input.{NoPosition, CharSequenceReader, CharArrayReader}

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  def read[A <: LispValue](expression: LispParser.Parser[A], input: String, module: Module = NilModule)
                          (f: A => LispValue): LispValue = {
    LispParser.setModule(module)
    LispParser.parseAll(expression, new CharArrayReader(input.toArray)) match {
      case Success(expr, _) => f(expr)
      case failure@Failure(expr, _) => Errors.parseFailure(failure.toString,  
        Some(failure.pos.line, failure.pos.column, failure.pos.longString))
      case error@Error(expr, _) => Errors.parseError(error.toString,
        Some(error.pos.line, error.pos.column, error.pos.longString))
    }
  }
  
  def evalExpression(str: String, environment: Environment): LispValue = 
    read(expression, str)(Evaluate(_, environment))
  
  def whenValid[A <: LispValue, B <: LispValue](lval: A)(f: A => B) = lval match {
    case error: LispError => error
    case lval => f(lval)
  }

  def checkArgsCount(args: LispList)(p: Int => Boolean)(f: => LispValue) =
    if (!p(args.size)) Errors.incorrectArgCount(args.size, args) else f

  def sanitizeAll(args: LispList, environment: Environment)(f: LispList => LispValue) =  
    whenValid(args.foldLeft[LispValue](NilLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(Evaluate(lval, environment))(_ !: llist)
      }
    }) {
      case lval: LispList => f(lval.reverse)
      case error: LispError => error
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  
  def withCollArg(args: LispList)(f: LispList => LispValue)(g: LispString => LispValue) = 
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
  
  def withPairListArgs(args: LispList)(f: => LispValue) = checkArgsCount(args)(_ > 0) {
    args.find(!isPair(_)).map(expr => Errors.invalidType(LispTypeStrings.Pair, expr)) getOrElse f 
  }
  
  def allSymbols(llist: LispList)(f: => LispValue) = llist.find {
    case _: LispSymbol => false
    case _ => true
  } map(lval => Errors.invalidType(LispTypeStrings.Symbol, lval)) getOrElse f
  
  def withStringArg(args: LispList, environment: Environment)(f: String => LispValue) = checkArgsCount(args)(_ == 1) {
    whenValid(Evaluate(args.head, environment)) {
      case LispString(str) => f(str)
      case lval => Errors.invalidType(LispTypeStrings.String, lval)
    }
  }
  
  def whenNumber[A](lval: LispValue)
                    (f: Int => A)
                    (g: Long => A)
                    (h: Float => A)
                    (k: Double => A): LispValue = lval match {
    case LispInt(x) => f(x)
    case LispLong(x) => g(x)
    case LispFloat(x) => h(x)
    case LispDouble(x) => k(x)
    case _ => Errors.invalidType(LispTypeStrings.Integer, lval)
  }
  
  def withSingleArg(args: LispList)(f: LispValue => LispValue): LispValue = 
    checkArgsCount(args)(_ == 1)(args match {
      case ConsLispList(expr, _) => f(expr)
    })
  
  def withAtLeastOneArg(args: LispList)(f: LispList => LispValue) = checkArgsCount(args)(_ > 0)(f(args))

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
    case x => Errors.Exceptions.invalidNumberType(x)
  }
  
  def isDefineCommand(str: String) = List(Keywords.Def, Keywords.Defun, Keywords.DefMacro) contains str
}
