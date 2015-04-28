package meruem

import meruem.Constants.LispTypeStrings

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
}
