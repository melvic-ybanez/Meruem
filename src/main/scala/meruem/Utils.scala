package meruem

import meruem.Constants.LispTypeStrings
import meruem.LispParser._

import scala.util.parsing.input.CharArrayReader

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  implicit def lispValueToBool(lval: LispValue): Boolean = lval.isTrue
  
  def read[A <: LispValue](expression: LispParser.Parser[A], str: String)(f: A => A): LispValue = 
    parseAll(expression, str) match {
      case Success(expr, _) => f(expr)
      case failure: Failure => Errors.parseFailure(failure.toString)
      case error: Error => Errors.parseError(error.toString)
    } 
  
  def evalExpression(str: String, environment: Environment): LispValue = read(expression, str)(Evaluate(_, environment))
  
  def whenValid[A <: LispValue, B <: LispValue](lval: A)(f: A => B) = lval match {
    case error: LispError => error
    case lval => f(lval)
  }

  def checkArgsCount(args: LispList)(p: Int => Boolean)(f: => LispValue) =
    if (!p(args.size)) Errors.incorrectArgCount(args.size) else f

  def sanitizeAll(args: LispList, environment: Environment)(f: LispList => LispValue) =  
    whenValid(args.foldLeft[LispValue](EmptyLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(Evaluate(lval, environment))(_ :: llist)
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
    case ConsLispList(_, ConsLispList(_, EmptyLispList)) => true
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
}
