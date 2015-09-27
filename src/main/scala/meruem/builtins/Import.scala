package meruem.builtins

import java.io.File
import java.nio.file._

import meruem.Constants.{Keywords, ModuleSymbol, _}
import meruem.Utils._
import meruem.builtins.Functions._
import meruem.{LispList, LispNil, LispValue, _}
import meruem.LispParser._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source

/**
 * Created by ybamelcash on 5/28/2015.
 */
object Import {
  def apply(args: LispList)(implicit callingEnv: Environment) = withStringArg(args) { filePath =>
    def newArg(path: String) = {
      val newPath = Paths.get(path).resolve(filePath).toString
      val newArg = filePathToModulePath(newPath)
      LispList(LispString(newArg))
    }
    
    // If the path isn't absolute, try making it relative to the current directory.
    // If it still fails, try making it relative to MERUEM_HOME.
    // If it still fails, return an error.
    // Warning: This implementation has an issue --- if there's an error in the source 
    // file this problem would think the file doesn't exist and would try the user.dir
    // and libLocation before throwing an error. One way to fix it is to make the error
    // messages type safe so we can determine via pattern matching the type of errors.
    doImport(args) match {
      case LispError(_, _) => doImport(newArg(System.getProperty("user.dir"))) match {
        case LispError(_, _) => doImport(newArg(Paths.get(Settings.libLocation).toString))
        case lval => lval
      }
      case lval => lval
    }
  }
  
private def doImport(args: LispList)
           (implicit callingEnv: Environment): LispValue = withStringArg(args) { filePath =>
    val callingModule = callingEnv.module
    val modulePath = Paths.get(filePath.replace(ModuleSeparator, File.separator)).normalize.toString
      
    if (Files.isDirectory(Paths.get(modulePath))) {
      val paths = Files.newDirectoryStream(Paths.get(modulePath)).asScala.toList.filter { path =>
        // Get all the files that are not directories and end with the correct file extension. 
        !Files.isDirectory(path) && path.toString.toLowerCase.endsWith(Settings.fileExtension)
      } map { path =>
        // Remove the file extension
        Paths.get(path.toString.dropRight(Settings.fileExtension.length))
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
      val extendedFilePath = modulePath + Settings.fileExtension
      
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
      } else Errors.fileNotFound(modulePath, args)
    }  
  }
}
