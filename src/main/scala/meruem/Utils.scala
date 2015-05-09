package meruem

import meruem.Constants.LispTypeStrings
import meruem.LispParser._

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  def read(str: String, environment: Environment): LispValue = parse(expression, str) match {
    case Success(expr, _) => Evaluate(expr, environment)
    case Failure(msg, _) => LispError(msg)
    case Error(msg, _) => LispError(msg)
  }
  
  def whenValid[A <: LispValue, B <: LispValue](args: A)(f: A => B) = args match {
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
}
