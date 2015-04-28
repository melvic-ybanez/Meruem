package meruem

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Errors {
  def unboundSymbol(symbol: LispSymbol) = LispError(s"Unbound symbol: $symbol.")
  
  def extraArgs(extraCount: Int) = 
    LispError(s"Extra number of arguments: $extraCount")
  
  def invalidType(expectedTypeString: String, actual: LispValue) = 
    LispError("Invalid Type. " +
      s"Expected: $expectedTypeString. " +
      s"Actual: ${Utils.typeString(actual)}")
  
  def invalidFormat(msg: String) = LispError(msg)
  
  def nonFunction(lval: LispValue) = LispError(s"$lval can not be converted to a function.")
  
  def incorrectArgCount(count: Int) = LispError(s"Incorrect number of arguments: $count")
  
  def divisionByZero = LispError("Division by zero")
}