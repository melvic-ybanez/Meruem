package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._
import meruem.builtins.Import
import meruem.builtins.Predicates._
import meruem.builtins.Relational._
import meruem.builtins.Conversions._
import meruem.builtins.io.{Path, Std}
import Std._
import Path._
import meruem.Constants._
import FunctionNames._
import Keywords._

import scala.collection.mutable

/**
 * Created by ybamelcash on 5/3/2015.
 */
object Globals {
  lazy val environment: Environment = SomeEnvironment(
    collection.mutable.Map(
      Add -> LispBuiltinFunction(add),
      Subtract -> LispBuiltinFunction(subtract),
      Multiply -> LispBuiltinFunction(multiply),
      Divide -> LispBuiltinFunction(divide),
      Modulus -> LispBuiltinFunction(modulus),
      Equals -> LispBuiltinFunction(equal),
      Not -> LispBuiltinFunction(not),
      LessThan -> LispBuiltinFunction(<),
      GreaterThan -> LispBuiltinFunction(>),
      Head -> LispBuiltinFunction(head),
      Tail -> LispBuiltinFunction(tail),
      Cons -> LispBuiltinFunction(cons),
      Cond -> LispCondSymbol,
      Quote -> LispQuoteSymbol,
      Quasiquote -> LispQuasiQuoteSymbol,
      Unquote -> LispUnquoteSymbol,
      List -> LispBuiltinFunction(list),
      AtomP -> LispBuiltinFunction(isAtom),
      SymbolP -> LispBuiltinFunction(isSymbol),
      ListP -> LispBuiltinFunction(isList),
      Macro -> LispBuiltinFunction(getMacro),
      LNil -> LispNil,
      True -> LispBoolean(true),
      False -> LispBoolean(false),
      Eval -> LispEvalSymbol,
      Read -> LispBuiltinFunction(read),
      Lambda -> LispLambdaSymbol,
      Keywords.Import -> LispBuiltinFunction(Import),
      Def -> LispDefSymbol,
      Defun -> LispDefunSymbol,
      Keywords.DefMacro -> LispDefMacroSymbol,
      LError -> LispBuiltinFunction(error),
      GetType -> LispBuiltinFunction(getType),
      ToInt -> LispBuiltinFunction(toInt),
      ToLong -> LispBuiltinFunction(toLong),
      ToFloat -> LispBuiltinFunction(toFloat),
      ToDouble -> LispBuiltinFunction(toDouble),
      ToString -> LispBuiltinFunction(toLispString),
      ReadLine -> LispBuiltinFunction(readLine),
      Print -> LispBuiltinFunction(print),
      ToPath -> LispBuiltinFunction(toPath),
      PathFileName -> LispBuiltinFunction(pathFileName),
      PathName -> LispBuiltinFunction(pathName),
      PathNameCount -> LispBuiltinFunction(pathNameCount),
      PathSubpath -> LispBuiltinFunction(subpath),
      PathGetParent -> LispBuiltinFunction(getParent),
      PathGetRoot -> LispBuiltinFunction(getRoot),
      PathNormalize -> LispBuiltinFunction(normalize),
      PathToURI -> LispBuiltinFunction(toURI),
      PathToAbsolute -> LispBuiltinFunction(toAbsolutePath),
      PathToReal -> LispBuiltinFunction(toRealPath),
      PathResolve -> LispBuiltinFunction(resolve),
      PathRelativize -> LispBuiltinFunction(relativize)
    ), 
    NilEnvironment
  )
  
  lazy val module = SomeModule(Settings.languageName + " Global", mutable.MutableList(), environment)
  
  val modules = mutable.MutableList[Module]()
}
