package meruem.builtins

import scala.collection.JavaConverters._

import java.nio.file._

import meruem.Constants.{LispTypeStrings, Keywords}
import meruem.{LispValue, LispNil, NilEnvironment, LispList}
import meruem.Utils._
import meruem._
import meruem.builtins.Functions._
import meruem.LispParser._

import scala.io.Source

/**
 * Created by ybamelcash on 5/28/2015.
 */
case object Import extends (LispList => LispValue) {
  def apply(args: LispList): LispValue = withStringArg(args, Globals.environment) { filePath =>
    if (Files.exists(Paths.get(filePath))) 
      if (Globals.modules.contains(filePath)) LispNil
      else if (Files.isDirectory(Paths.get(filePath), LinkOption.NOFOLLOW_LINKS)) {
        val paths = Files.newDirectoryStream(Paths.get(filePath)).asScala.toList.filter(!Files.isDirectory(_))
        
        def recurse(paths: List[Path], modules: LispList): Either[LispError, LispList] = paths match {
          case Nil => Right(modules)
          case path :: tail => Import(LispString(path.toString) !: NilLispList) match {
            case error: LispError => Left(error)
            case LispNil => recurse(tail, modules)
            case module: Module => recurse(tail, module !: modules)
          }
        } 
        
        recurse(paths, NilLispList) match {
          case Left(error) => error
          case Right(modules) => modules
        }
      } else {
        val environment = SomeEnvironment(collection.mutable.Map(), Globals.environment)
        /** This is the module instance we are going to create and return. */
        def module: Module = SomeModule(filePath, modules, environment)

        def exprs = Utils.read(meruem, Source.fromFile(filePath).mkString, module)(identity) match {
          case llist: LispList => llist
        }
        
        def evalExprs(exprs: LispList, modules: LispList): Either[LispError, LispList] = exprs match {
          case NilLispList => Right(modules)

          // If it's an import expression, apply the Import function to it.
          case (LispSymbol(Keywords.Import) !: args) !: tail => Import(args) match {
            case error: LispError => Left(error)
            case LispNil => evalExprs(tail, modules)
            case module: Module => evalExprs(tail, module !: modules)
            case modules1: LispList => evalExprs(tail, modules ++ modules1)
          }

          // If it's a define statement, evaluate it's args and register it to the environment.  
          case (LispSymbol(op) !: args) !: tail if isDefineCommand(op) => 
            val func = { (x: LispList, e: Environment) => op match {
              case Keywords.Def => define(x, e) 
              case Keywords.Defun => defun(x, e)
              case Keywords.DefMacro => defmacro(x, e)
            }}
            func(args, environment) match {
              case error: LispError => Left(error)
              case _ => evalExprs(tail, modules)
            }

          // If the expression is neither import nor define, just evaluate it.  
          case lval !: tail => Evaluate(lval, environment) match {
            case error: LispError => Left(error)
            case _ => evalExprs(tail, modules)
          }
        }
        
        lazy val (modules, errorOpt) = evalExprs(exprs, NilLispList) match {
          case Left(error) => (NilLispList, Some(error))
          case Right(modules) => (modules, None)
        }
        
        errorOpt map { error =>
          Globals.modules.clear()
          error
        } getOrElse {
          Globals.modules += filePath
          module
        }
      }
    else Errors.fileNotFound(filePath, args)
  }
}
