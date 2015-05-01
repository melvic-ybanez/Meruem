package meruem

import meruem.Constants.LispTypeStrings
import meruem.Utils.Aliases._

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  object Aliases {
    type LispValueList = LispList[LispValue]
    type LispSymbolList = LispList[LispSymbol]
    type LispNumberList = LispList[LispNumber]
    type LispString = LispList[Char]
  }
  
  def typeString(lval: LispValue) = lval match {
    case _: LispSymbol => LispTypeStrings.Symbol
    case _: LispNumber => LispTypeStrings.Number
    case _: LispString => LispTypeStrings.String
    case _: LispList => LispTypeStrings.List
    case _: LispError => LispTypeStrings.Error
  }
  
  def whenValid[A <: LispValue, B <: LispValue](args: A)(f: A => B) = args match {
    case error: LispError => error
    case lval: A => f(lval)
  }

  def checkArgsCount(args: LispValueList)(p: Int => Boolean)(f: => LispValue) =
    if (p(args.size)) Errors.incorrectArgCount(args.size) else f

  def sanitizeAll[A <: LispValue](args: LispValueList)(f: LispList[A] => LispValue) =  
    whenValid(args.foldLeft[LispValue](EmptyLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(lval.evaluate)(_ :: llist)
        case _ => 
      }
    }) {
      case lval: LispList[A] => f(lval)
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  
  def hasListArg(args: LispValueList)(f: LispValueList => LispValue) = checkArgsCount(args)(_ == 1) {
    whenValid(args.head) {
      case llist: LispList[LispValue] => f(llist)
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  }
}
