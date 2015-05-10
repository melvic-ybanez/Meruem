package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._
import meruem.Constants._

/**
 * Created by ybamelcash on 5/3/2015.
 */
object Globals {
  lazy val environment = NonEmptyEnvironment(
    Map(
      "+" -> LispBuiltinFunction(add),
      "-" -> LispBuiltinFunction(subtract),
      "*" -> LispBuiltinFunction(multiply),
      "/" -> LispBuiltinFunction(divide),
      "head" -> LispBuiltinFunction(head),
      "tail" -> LispBuiltinFunction(tail),
      "=" -> LispBuiltinFunction(equal),
      "cons" -> LispBuiltinFunction(cons),
      "cond"-> LispCondSymbol,
      "quote" -> LispQuoteSymbol,
      "list" -> LispBuiltinFunction(list),
      "atom?" -> LispBuiltinFunction(atom),
      "nil" -> LispNil,
      "true" -> LispBoolean(true),
      "false" -> LispBoolean(false),
      "read" -> LispReadSymbol,
      "def" -> LispDefSymbol,
      "lambda" -> LispLambdaSymbol,
      "error" -> LispBuiltinFunction(error)
    ), 
    EmptyEnvironment
  )
}
