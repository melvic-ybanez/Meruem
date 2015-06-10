package meruem.builtins

import java.io.File

import scala.collection.JavaConverters._

import java.nio.file._

import meruem.Constants.{LispTypeStrings, Keywords, LispModuleSymbol}
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
  def apply(args: LispList)(implicit callingEnv: Environment): LispValue = withStringArg(args) { filePath =>
    val callingModule = callingEnv.module
    val callingModuleParentPath = Paths.get(callingModule.filePath).getParent
    val modulePath = callingModuleParentPath + File.separator + filePath.replace(ModuleSeparator, File.separator)
    
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
          
          environment += (LispModuleSymbol, module)

          Globals.modules += module
          callingModule.submodules += module

          def exprs = Utils.read(meruem, Source.fromFile(extendedFilePath).mkString)(identity) match {
            case llist: LispList => llist
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

          evalExprs(exprs, NilLispList) match {
            case Left(error) =>
              Globals.modules.clear()
              error
            case Right(modules) =>
              module.submodules ++= modules
              module
          }
        }
      } else Errors.fileNotFound(modulePath, args)
    }  
  }
}
