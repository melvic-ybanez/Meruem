package meruem.builtins

import meruem.Constants.{LispTypeStrings, Keywords}
import meruem.Implicits._
import meruem.Utils._
import meruem._
import Functions._
import meruem.LispParser._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

import java.nio.file.{Paths, Files}

/**
 * Created by ybamelcash on 5/26/2015.
 */
case object Include extends ((LispList, Environment) => LispValue) {
  // We need to include the input source and the filepath of every expressions.
  // We need them for the tracing of errors later on. The first string represents the parser input,
  // the second one would be the filepath.
  type LispListWithSources = List[(LispValue, String, String)]
  type LoadResult = (LispListWithSources, Option[LispError])

  def preInclude(path: String, pathsIncluded: List[String]): LoadResult = {
    if (Files.exists(Paths.get(path))) {
      def parseError(f: Option[String] => LispError) = (Nil, Some(f(Some(path))))

      def recurseRead(input: LispParser.Input, acc: LispListWithSources): LoadResult =
        if (input.atEnd) (acc, None)
        else parse(expression, input) match {
          case success @ Success(expr, next) => expr match {
            // If a "load" expression is found, perform a recursive expansion  
            case ConsLispList(LispSymbol(Keywords.Include), ConsLispList(LispString(path1), EmptyLispList)) =>
              // If a path has already been loaded, skip it. Otherwise, expand it.
              if (pathsIncluded.contains(path1) || Settings.preloads.contains(path1)) recurseRead(next, acc)
              else preInclude(path1, path :: pathsIncluded) match {
                case (loadedExprs, None) => recurseRead(next, loadedExprs ++ acc)
                case preloadResult => preloadResult
              }

            case _ => recurseRead(next, (expr, success.toString, path) :: acc)
          }
          case failure: Failure => parseError(Errors.parseFailure(failure.toString, _))
          case error: Error => parseError(Errors.parseError(error.toString, _))
        }

      recurseRead(new CharSequenceReader(Source.fromFile(path).mkString), Nil)
    } else (Nil, Some(Errors.fileNotFound(path)))
  }
  
  def apply(args: LispList, environment: Environment) = withStringArg(args, environment) { path =>
    preInclude(path, Nil) match {
      case (_, Some(error)) => error
      case (exprsWithSources, None) =>
        // Check if there are function and macro declarations 
        // (i.e list expressions that start with either "defun" or "defmacro")
        def recurse(exprsWithSources: LispListWithSources,
                    environment: Environment,
                    funcsWithSources: LispListWithSources): (LoadResult, Environment) = exprsWithSources match {
          case Nil => ((funcsWithSources, None), environment)
          case (expr, source, path) :: tail => expr match {
            case llist @ ConsLispList(LispSymbol(sym), _)
              if sym == Keywords.Defun || sym == Keywords.DefMacro =>
              recurse(tail, environment, (llist, source, path) :: funcsWithSources)
            case _ => Evaluate(expr, environment) match {
              case LispError(msg, _) => ((Nil, Some(Errors.withSource(msg, source, path))), environment)
              case LispDef(envi) => recurse(tail, envi, funcsWithSources)
              case lval => recurse(tail, environment, funcsWithSources)
            }
          }
        }

        recurse(exprsWithSources, environment, Nil) match {
          case ((_, Some(error)), _) => error
          case ((funcsWithSources, _), updatedEnvi) =>
            // Remember: LispDef evaluates its constructor arguments lazily.
            def ldef: LispDef = LispDef(newEnvironment)

            // Evaluate each of the functions and macros found and register each of them to the environment.
            // We need to update only the valueMaps at first, then pass it as an argument to the new environment.
            // The purpose is so that we wouldn't have to create a new environment everytime we register a new function
            // or macro, which is dangerous since an environment is immutable and we might end up having our functions
            // and macros point to different environments. So, the map gets constructed first, then we pass it to a single
            // environment, then have all those functions and macros point to that environment. 
            lazy val (newEnvironment, errorOpt) = {
              def recurse(funcsWithSources: LispListWithSources,
                          values: Map[String, LispValue]): (Map[String, LispValue], Option[LispError]) = {
                def returnError(error: String, s: String, p: String) = (values, Some(Errors.withSource(error, s, p)))

                funcsWithSources match {
                  case Nil => (values, None)
                  case (llist: LispList, source, path) :: _ if llist.size != 4 =>
                    returnError(Errors.incorrectArgCount(llist.size - 1), source, path)
                  case (ConsLispList(LispSymbol(op),
                  ConsLispList(nameSym@LispSymbol(name),
                  ConsLispList(params, ConsLispList(body, _)))), source, path) :: tail =>
                    lambda(params :: body :: EmptyLispList, environment) match {
                      case llambda: LispLambda =>
                        if (values.exists(_._1 == name))
                          returnError(Errors.alreadyDefined(nameSym), source, path)
                        else recurse(tail, values + (name -> {
                          val llambda1 = llambda.updated(environment = ldef.environment)
                          if (op == "defun") llambda1 else LispDefMacro(llambda1)
                        }))
                      case LispError(msg, _) => returnError(msg, source, path)
                    }
                  case (ConsLispList(_, ConsLispList(name, _)), source, path) :: _ =>
                    returnError(Errors.invalidType(LispTypeStrings.Symbol, name), source, path)
                }
              }

              val (valueMap, errorOpt) = recurse(funcsWithSources, updatedEnvi.valueMap)
              (SomeEnvironment(valueMap, updatedEnvi.parent), errorOpt)
            }

            errorOpt.getOrElse(ldef)

          case (_, envi) => LispDef(envi)
        }
    }
  }
}
