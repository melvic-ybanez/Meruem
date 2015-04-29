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
  }
  
  def typeString(lval: LispValue) = lval match {
    case _: LispSymbol => LispTypeStrings.Symbol
    case _: LispString => LispTypeStrings.String
    case _: LispNumber => LispTypeStrings.Number
    case _: LispList => LispTypeStrings.List
    case _: LispError => LispTypeStrings.Error
  }
  
  def whenValid[A <: LispValue, B <: LispValue](args: A)(f: A => B) = args match {
    case error: LispError => error
    case lval: A => f(lval)
  }

  def checkArgsCount(args: LispValueList, n: Int)(f: => LispValue) =
    if (args.size != n) Errors.incorrectArgCount(args.size) else f

  def requireArgs(args: LispValueList)(f: => LispValue) = args match {
    case EmptyEnvironment => Errors.incorrectArgCount(0)
    case _ => f
  }

  def sanitizeAll(args: LispValueList)(f: LispValue => LispValue) =  
    whenValid(args.foldLeft[LispValue](EmptyLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(lval.evaluate)(_ :: llist)
        case _ => 
      }
    })(f)
  
  def withListArg(args: LispValueList)(f: LispValueList => LispValue) = 
    checkArgsCount(args, 1)(sanitizeAll(args) match {
      case llist: LispList => llist.head match {
        case llist: LispList[LispValue] => f(llist)
        case error: LispError => error
      }
      case error: LispError => error
    })
}
