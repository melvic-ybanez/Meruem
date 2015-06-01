package meruem.builtins

import scala.collection.JavaConverters._

import java.nio.file._

import meruem.Constants.{LispTypeStrings, Keywords}
import meruem.{LispValue, LispNil, NilEnvironment, LispList}
import meruem.Environment._
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
    type LispValues = List[LispValue]
    type Modules = List[Module]
    
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
        /** This is the module instance we are going to create and return. */
        def module: Module = SomeModule(filePath, modules, environment)

        def llist = Utils.read(meruem, Source.fromFile(filePath).mkString, module)(identity).specify
        
        /** The list of modules we are going to pass as argument to complete the creation of 
          * the module.
          * 
          * We need this to be lazy to avoid cyclic reference (i.e module needs the modules 
          * argument, which will be created after parsing and evaluation, which can't happen
          * without the module).
          */
        lazy val (errorOpt, modules, environment) = { 
          /** This function gets all the import and define statments (def, defun, defmacro). 
            * Any other expressions will be evaluated.
            */
          def preLoad(exprs: LispList,
                      modules: Modules,
                      defines: LispValues,
                      funcs: LispValues): Either[LispError, (Modules, LispValues, LispValues)] = exprs match {
            case NilLispList => Right(modules, defines, funcs)

            // If it's an import expression, apply the Import function to it.
            case (LispSymbol(Keywords.Import) !: args) !: tail => Import(args) match {
              case error: LispError => Left(error)
              case LispNil => preLoad(tail, modules, defines, funcs)
              case module: Module => preLoad(tail, module :: modules, defines, funcs)
              case modules1: Modules => preLoad(tail, modules ++ modules1, defines, funcs)
            }

            // If it's a define expression, save it for later.
            case define@((LispSymbol(sym) !: args) !: tail) if isDefineCommand(sym) =>
              if (sym == Keywords.Def) preLoad(tail, modules, define :: defines, funcs)
              else preLoad(tail, modules, defines, define :: funcs)

            // If the expression is neither import nor define, just evaluate it.  
            case lval !: tail => Evaluate(lval, Globals.environment) match {
              case error: LispError => Left(error)
              case lval => preLoad(tail, modules, defines, funcs)
            }
          }

          preLoad(llist, Nil, Nil, Nil) match {
            case Left(error) => (Some(error), Nil, NilEnvironment)
            case Right((modules, defines, funcs)) => 
              def ldef: LispDef = LispDef(Globals.environment)

              def createEnvironment: (Environment, Option[LispError]) = {
                /** Evaluate all the defun and defmacro expressions. */
                def evalFuncExprs(funcs: LispValues, valueMap: ValueMapType): Either[LispError, ValueMapType] = funcs match {
                  case Nil => Right(valueMap)
                  case (LispSymbol(op) !: (nameSym@LispSymbol(name)) !: params !: body) :: tail =>
                    lambda(params !: body, Globals.environment) match {
                      case llambda: LispLambda =>
                        // Make sure the name does not exist.
                        if (valueMap.exists(_._1 == name)) Left(Errors.alreadyDefined(nameSym))
                        else evalFuncExprs(tail, valueMap + (name -> {
                          val llambda1: LispLambda = llambda
                          if (op == Keywords.Defun) llambda1 else LispDefMacro(llambda1)
                        }))
                      case error: LispError => Left(error)
                    }
                  case (_ !: name !: _) :: _ => Left(Errors.invalidType(LispTypeStrings.Symbol, name))
                }

              
                evalFuncExprs(funcs, Globals.environment.valueMap) match {
                  case Left(error) => (NilEnvironment, Some(error))
                  case Right(valueMap) => (SomeEnvironment(valueMap, Globals.environment), None)
                }
              }
              
              lazy val result: (Environment, Option[LispError]) = createEnvironment
    
              result._2.map(err => (Some(err), Nil, NilEnvironment)).getOrElse {
                /** Evaluate all the def expressions. */
                def evalDefExprs(defs: LispValues, environment: Environment): Either[LispError, Environment] = defs match {
                  case Nil => Right(environment)
                  case (LispSymbol(Keywords.Def) !: (nameSym@LispSymbol(name)) !: args) :: tail =>
                    define(args, environment) match {
                      case error: LispError => Left(error)
                      case LispDef(env) => evalDefExprs(tail, env)
                    }
                  case (_ !: name !: _) :: _ => Left(Errors.invalidType(LispTypeStrings.Symbol, name))
                }
                
                evalDefExprs(defines, ldef.environment) match {
                  case Left(error) => (Some(error), Nil, NilEnvironment)
                  case Right(env) => (None, modules, env)
                }
              }
          }
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
