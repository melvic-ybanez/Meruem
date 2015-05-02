package meruem

import meruem.Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  def typeString(lval: LispValue) = lval match {
    case _: LispSymbol => LispTypeStrings.Symbol
    case _: LispNumber => LispTypeStrings.Number
    case _: LispList => LispTypeStrings.List
    case _: LispError => LispTypeStrings.Error
  }
  
  def whenValid[A <: LispValue, B <: LispValue](args: A)(f: A => B) = args match {
    case error: LispError => error
    case lval => f(lval)
  }

  def checkArgsCount(args: LispList)(p: Int => Boolean)(f: => LispValue) =
    if (p(args.size)) Errors.incorrectArgCount(args.size) else f

  def sanitizeAll(args: LispList)(f: LispList => LispValue) =  
    whenValid(args.foldLeft[LispValue](EmptyLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(lval.evaluate)(_ :: llist)
      }
    }) {
      case lval: LispList => f(lval)
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  
  def hasListArg(args: LispList)(f: LispList => LispValue) = checkArgsCount(args)(_ == 1) {
    whenValid(args.head) {
      case llist: LispList => f(llist)
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  }
}
