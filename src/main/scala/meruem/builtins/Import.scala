package meruem.builtins

import java.io.File

import scala.collection.JavaConverters._

import java.nio.file._

import meruem.Constants.{LispTypeStrings, Keywords, ModuleSymbol}
import meruem.{LispValue, LispNil, NilEnvironment, LispList}
import meruem.Utils._
import meruem.Constants._
import meruem._
import meruem.builtins.Functions._
import meruem.Implicits.lispListToList
import meruem.LispParser._

import scala.collection.mutable
import scala.io.Source

/**
 * Created by ybamelcash on 5/28/2015.
 */
object Import {
  def apply(args: LispList)(implicit callingEnv: Environment) = doImport(args)
  
  private def doImport(args: LispList)
           (implicit callingEnv: Environment, 
            isRelative: Boolean = true): LispValue = withStringArg(args) { filePath =>
    val callingModule = callingEnv.module
    val callingModuleParentPath = Option( 
      if (isRelative) Paths.get(callingModule.filePath).getParent
      else Paths.get(Settings.libLocation).getParent)
    val modulePath = Paths.get(
      callingModuleParentPath.map(_ + File.separator).getOrElse("") + 
      filePath.replace(ModuleSeparator, File.separator)).normalize().toString
    
    if (Files.isDirectory(Paths.get(modulePath))) {
      val paths = Files.newDirectoryStream(Paths.get(modulePath)).asScala.toList.filter { path =>
        // Get all the files that are not directories and end with the correct file extension. 
        !Files.isDirectory(path) && path.toString.toLowerCase.endsWith(Settings.fileExtendsion)
      } map { path =>
        // Remove the file extension
        Paths.get(path.toString.dropRight(Settings.fileExtendsion.length))
      }
      
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
      val extendedFilePath = modulePath + Settings.fileExtendsion
      
      if (Files.exists(Paths.get(extendedFilePath))) {
        def moduleExists(module: Module) = module.filePath == modulePath
        
        if (callingModule.submodules.exists(moduleExists)) LispNil
        else Globals.modules.find(moduleExists).map { module =>
          callingModule.submodules += module
          LispNil
        } getOrElse {
          implicit val environment = SomeEnvironment(collection.mutable.Map(), Globals.environment)
          val module: Module = SomeModule(modulePath, mutable.MutableList(), environment)
          
          environment += (ModuleSymbol, module)

          Globals.modules += module
          callingModule.submodules += module
          
          def returnError(error: LispError) = {
            Globals.modules.clear()
            error
          }

          def evalExprs(exprs: LispList, modules: LispList): Either[LispError, LispList] = exprs match {
            case NilLispList => Right(modules)

            // If it's an import expression, apply the Import function to the arguments.
            case (LispSymbol(Keywords.Import) !: args) !: tail => Import(args) match {
              case error: LispError => Left(error)
              case LispNil => evalExprs(tail, modules)
              case module: Module => evalExprs(tail, module !: modules)
              case modules1: LispList => evalExprs(tail, modules ++ modules1)
            }

            // If it's a define statement, evaluate it's args and register it to the environment.  
            case (LispSymbol(op) !: args) !: tail if isDefineCommand(op) =>
              def func = { (x: LispList) => 
                op match {
                  case Keywords.Def => define(x)
                  case Keywords.DefMacro => defmacro(x)
                }
              }
              func(args) match {
                case error: LispError => Left(error)
                case _ => evalExprs(tail, modules)
              }

            // If the expression is neither import nor define, just evaluate it.  
            case lval !: tail => Evaluate(lval) match {
              case error: LispError => Left(error)
              case _ => evalExprs(tail, modules)
            }
          }

          // Create the import string. e.g '(import "prelude.mer")'
          val preloadString = 
            if (filePath == Globals.preloadedString) "" 
            else s"""$OpenParen${Keywords.Import} "${Globals.preloadedString}"$CloseParen"""

          Utils.read(meruem, preloadString + Source.fromFile(extendedFilePath).mkString)(identity) match {
            case error: LispError => returnError(error)
            case exprs: LispList => evalExprs(exprs, NilLispList) match {
              case Left(error) => returnError(error)
              case Right(modules) => module
            }
          }
        }
      } else if (isRelative) doImport(args)(callingEnv, false)
      else Errors.fileNotFound(modulePath, args)
    }  
  }
}
