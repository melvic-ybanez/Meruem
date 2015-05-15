package meruem

import meruem.Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Errors {
  def unboundSymbol(symbol: LispSymbol) = LispError(s"Unbound symbol: $symbol.")
  
  def extraArgs(args: LispList) = 
    LispError(s"Extra arguments: $args")
  
  def notEnoughArgs(params: LispList) = 
    LispError(s"Not enough arguments. Expected values for $params.")
  
  def invalidType(expectedTypeString: String, actual: LispValue) = 
    LispError(s"Invalid Type. Not a $expectedTypeString: $actual")
  
  def invalidFormat(msg: String) = LispError(msg)
  
  def nonFunction(lval: LispValue) = LispError(s"$lval can not be converted to a function.")
  
  def incorrectArgCount(count: Int) = LispError(s"Incorrect number of arguments: $count")
  
  def divisionByZero = LispError("Division by zero")
  
  def alreadyDefined(sym: LispSymbol) = LispError(s"$sym is already defined.")
  
  def varArgsCount = 
    Errors.invalidFormat(s"${LispTypeStrings.Symbol} ${Constants.VarArgsChar} is not followed by a single symbol.")
  
  def unquoteNotAllowed = 
    LispError(s"${LispTypeStrings.Unquote} can only be used inside a ${LispTypeStrings.Quasiquote}") 
}
