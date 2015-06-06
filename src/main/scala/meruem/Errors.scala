package meruem

import meruem.Constants.LispTypeStrings

import scala.util.parsing.input.Position

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Errors {
  def unboundSymbol(symbol: LispSymbol)(implicit env: Environment) = LispError(s"Unbound symbol: $symbol.", symbol)
  
  def extraArgs(args: LispList)(implicit env: Environment) = LispError(s"Extra arguments: $args", args)
  
  def notEnoughArgs(params: LispList)(implicit env: Environment) = 
    LispError(s"Not enough arguments. Expected values for $params.", params)
  
  def invalidType(expectedTypeString: String, actual: LispValue)(implicit env: Environment) = 
    LispError(s"Invalid Type. Not a $expectedTypeString: $actual", actual)
  
  def invalidFormat(msg: String, lval: LispValue)(implicit env: Environment) = LispError(msg, lval)
  
  def nonFunction(lval: LispValue)(implicit env: Environment) = 
    LispError(s"$lval can not be converted to a function.", lval)
  
  def incorrectArgCount(count: Int, lval: LispValue)(implicit env: Environment) = 
    LispError(s"Incorrect number of arguments: $count", lval)
  
  def divisionByZero(lval: LispValue)(implicit env: Environment) = LispError("Division by zero", lval)
  
  def alreadyDefined(sym: LispSymbol)(implicit env: Environment) = LispError(s"$sym is already defined.", sym)
  
  def varArgsCount(lval: LispValue)(implicit env: Environment) = Errors.invalidFormat(
    s"${LispTypeStrings.Symbol} ${Constants.VarArgsChar} is not followed by a single symbol.", lval)
  
  def unquoteNotAllowed(lval: LispValue)(implicit env: Environment) = 
    LispError(s"${LispTypeStrings.Unquote} can only be used inside a ${LispTypeStrings.Quasiquote}", lval)
  
  def fileNotFound(filename: String, lval: LispValue)(implicit env: Environment) = 
    LispError("File Not Found: " + filename, lval)
  
  def parseFailure(msg: String, position: Position)(implicit env: Environment) = 
    LispError("Parse Failure: " + msg, dummyLispValue(position))
  
  def parseError(msg: String, position: Position)(implicit env: Environment) = 
    LispError("Parse Error: " + msg, dummyLispValue(position))
  
  def unableToParse(msg: String, lval: LispValue)(implicit env: Environment) = 
    LispError("Unable to parse: " + msg, lval)
  
  def alreadyLoaded(path: String, lval: LispValue)(implicit env: Environment) =
    LispError("Already loaded: " + path, lval)
  
  def unrecognizedType(lval: LispValue)(implicit env: Environment) = LispError("Unrecognized Type: " + lval, lval)
  
  object Exceptions {
    def invalidNumberType(any: Any) = throw new IllegalArgumentException("Invalid number type " + any)
  }
  
  def dummyLispValue(pos: Position) = LispNil.setPos(pos)
}
