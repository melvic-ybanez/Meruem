package meruem

import meruem.Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Errors {
  def unboundSymbol(symbol: LispSymbol) = LispError(s"Unbound symbol: $symbol.", symbol)
  
  def extraArgs(args: LispList) = 
    LispError(s"Extra arguments: $args", args)
  
  def notEnoughArgs(params: LispList) = 
    LispError(s"Not enough arguments. Expected values for $params.", params)
  
  def invalidType(expectedTypeString: String, actual: LispValue) = 
    LispError(s"Invalid Type. Not a $expectedTypeString: $actual", actual)
  
  def invalidFormat(msg: String, lval: LispValue) = LispError(msg, lval)
  
  def nonFunction(lval: LispValue) = LispError(s"$lval can not be converted to a function.", lval)
  
  def incorrectArgCount(count: Int, lval: LispValue) = LispError(s"Incorrect number of arguments: $count", lval)
  
  def divisionByZero(lval: LispValue) = LispError("Division by zero", lval)
  
  def alreadyDefined(sym: LispSymbol) = LispError(s"$sym is already defined.", sym)
  
  def varArgsCount(lval: LispValue) = Errors.invalidFormat(
    s"${LispTypeStrings.Symbol} ${Constants.VarArgsChar} is not followed by a single symbol.", lval)
  
  def unquoteNotAllowed(lval: LispValue) = 
    LispError(s"${LispTypeStrings.Unquote} can only be used inside a ${LispTypeStrings.Quasiquote}", lval)
  
  def fileNotFound(filename: String, lval: LispValue) = LispError("File Not Found: " + filename, lval)
  
  def parseFailure(msg: String, position: Option[(Int, Int, String)]) = LispError("Parse Failure: " + msg, LispNil, position)
  
  def parseError(msg: String, position: Option[(Int, Int, String)]) = LispError("Parse Error: " + msg, LispNil, position)
  
  def unableToParse(msg: String, lval: LispValue) = LispError("Unable to parse: " + msg, lval)
  
  def alreadyLoaded(path: String, lval: LispValue) = LispError("Already loaded: " + path, lval)
  
  def unrecognizedType(lval: LispValue) = LispError("Unrecognized Type: " + lval, lval)
  
  object Exceptions {
    def invalidNumberType(any: Any) = throw new IllegalArgumentException("Invalid number type " + any)
  }
}
