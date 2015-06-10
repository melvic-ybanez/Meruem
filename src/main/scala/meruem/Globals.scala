package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._
import meruem.builtins.Import
import meruem.builtins.Predicates._
import meruem.builtins.Relational._
import meruem.builtins.Conversions._
import meruem.builtins.io.{Path, Std, File}
import Std._
import Path._
import File._
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
      IsAtom -> LispBuiltinFunction(isAtom),
      IsSymbol -> LispBuiltinFunction(isSymbol),
      IsList -> LispBuiltinFunction(isList),
      Macro -> LispBuiltinFunction(getMacro),
      LNil -> LispNil,
      True -> LispBoolean(true),
      False -> LispBoolean(false),
      Eval -> LispBuiltinFunction(eval),
      Read -> LispBuiltinFunction(read),
      Lambda -> LispLambdaSymbol,
      Keywords.Import -> LispImportSymbol,
      Def -> LispDefSymbol,
      Keywords.DefMacro -> LispDefMacroSymbol,
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
      PathsName -> LispBuiltinFunction(pathName),
      PathsNameCount -> LispBuiltinFunction(pathNameCount),
      PathsSubpath -> LispBuiltinFunction(subpath),
      PathsGetParent -> LispBuiltinFunction(getParent),
      PathsGetRoot -> LispBuiltinFunction(getRoot),
      PathsNormalize -> LispBuiltinFunction(normalize),
      PathsToURI -> LispBuiltinFunction(toURI),
      PathsToAbsolute -> LispBuiltinFunction(toAbsolutePath),
      PathsToReal -> LispBuiltinFunction(toRealPath),
      PathsResolve -> LispBuiltinFunction(resolve),
      PathsRelativize -> LispBuiltinFunction(relativize),
      FilesExists -> LispBuiltinFunction(exists),
      FilesIsReadable -> LispBuiltinFunction(isReadable),
      FilesIsWritable -> LispBuiltinFunction(isWriteable),
      FilesIsExecutable -> LispBuiltinFunction(isExecuatable),
      FilesDelete -> LispBuiltinFunction(delete),
      FilesCopy -> LispBuiltinFunction(copy),
      FilesSize -> LispBuiltinFunction(size),
      FilesIsDirectory -> LispBuiltinFunction(isDirectory),
      FilesIsHidden -> LispBuiltinFunction(isHidden),
      IsError -> LispIsErrorSymbol,
      TryCatch -> LispTryCatchSymbol,
      Error -> LispBuiltinFunction(error)
    ), 
    NilEnvironment
  )
  
  lazy val module = SomeModule(Settings.libLocation + java.io.File.separator + Settings.languageName + " Global", mutable.MutableList(), environment)
  
  val modules = mutable.MutableList[Module]()
}
