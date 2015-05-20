package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._
import meruem.Constants._

/**
 * Created by ybamelcash on 5/3/2015.
 */
object Globals {
  lazy val environment = SomeEnvironment(
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
      "quasiquote" -> LispQuasiQuoteSymbol,
      "unquote" -> LispUnquoteSymbol,
      "list" -> LispBuiltinFunction(list),
      "atom?" -> LispBuiltinFunction(isAtom),
      "symbol?" -> LispBuiltinFunction(isSymbol),
      "list?" -> LispBuiltinFunction(isList),
      "macro" -> LispBuiltinFunction(getMacro),
      "nil" -> LispNil,
      "true" -> LispBoolean(true),
      "false" -> LispBoolean(false),
      "eval" -> LispReadSymbol,
      "read" -> LispBuiltinFunction(read),
      Keywords.Load -> LispLoadSymbol,
      "def" -> LispDefSymbol,
      Keywords.Defun -> LispDefunSymbol,
      Keywords.DefMacro -> LispLambdaSymbol,
      "error" -> LispBuiltinFunction(error),
      "defmacro" -> LispDefMacroSymbol
    ), 
    NilEnvironment
  )
}
