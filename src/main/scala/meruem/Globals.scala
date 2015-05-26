package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._
import meruem.builtins.Predicates._
import meruem.builtins.Conversions._
import meruem.builtins.Relational._
import meruem.Constants._

/**
 * Created by ybamelcash on 5/3/2015.
 */
object Globals {
  lazy val environment = SomeEnvironment(
    Map(
      FunctionNames.Add -> LispBuiltinFunction(add),
      FunctionNames.Subtract -> LispBuiltinFunction(subtract),
      FunctionNames.Multiply -> LispBuiltinFunction(multiply),
      FunctionNames.Divide -> LispBuiltinFunction(divide),
      FunctionNames.Modulus -> LispBuiltinFunction(modulus),
      FunctionNames.Equals -> LispBuiltinFunction(equal),
      FunctionNames.Not -> LispBuiltinFunction(not),
      FunctionNames.LessThan -> LispBuiltinFunction(<),
      FunctionNames.GreaterThan -> LispBuiltinFunction(>),
      "head" -> LispBuiltinFunction(head),
      "tail" -> LispBuiltinFunction(tail),
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
      Keywords.Include -> LispLoadSymbol,
      "def" -> LispDefSymbol,
      Keywords.Defun -> LispDefunSymbol,
      Keywords.DefMacro -> LispLambdaSymbol,
      "error" -> LispBuiltinFunction(error),
      "defmacro" -> LispDefMacroSymbol,
      FunctionNames.GetType -> LispBuiltinFunction(getType),
      FunctionNames.ToInt -> LispBuiltinFunction(toInt),
      FunctionNames.ToLong -> LispBuiltinFunction(toLong),
      FunctionNames.ToFloat -> LispBuiltinFunction(toFloat),
      FunctionNames.ToDouble -> LispBuiltinFunction(toDouble)
    ), 
    NilEnvironment
  )
}
