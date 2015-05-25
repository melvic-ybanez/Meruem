package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Constants._
import meruem.Implicits._
import meruem._
import meruem.Utils._
import meruem.LispParser._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def macroExpand(lval: LispValue, environment: Environment): LispValue = lval match {
    case ConsLispList(LispDefMacro(func), tail) =>
      macroExpand(
        Evaluate(func.updated(args = tail.map(x => LispList(LispQuoteSymbol, x))), environment), 
        environment)
    case _ => lval
  }
  
  def defmacro(args: LispList, environment: Environment) = defineFunction(args, environment)(LispDefMacro(_))
  
  def getMacro(args: LispList) = checkArgsCount(args)(_ == 1) {
    args.head match {
      case lmacro: LispDefMacro => lmacro
      case _ => LispNil
    }
  }

  def lambda(args: LispList, environment: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case ConsLispList(llist: LispList, ConsLispList(body, _)) => allSymbols(llist) {
      LispLambda(llist, EmptyLispList, body, SomeEnvironment(Map(), environment))
    }
  })
  
  def define(args: LispList, environment: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case ConsLispList(sym: LispSymbol, ConsLispList(value, _)) => 
      // Check whether the symbol has already been defined or not
      environment.whenNotdefined(sym) {
        whenValid(Evaluate(value, environment)) {
          case lval => LispDef(environment +(sym, lval))
        }
      }
    case ConsLispList(lval, _) => Errors.invalidType(LispTypeStrings.Symbol, lval)
  })
  
  def defun(args: LispList, environment: Environment) = defineFunction(args, environment)(llambda => llambda)

  def defineFunction(args: LispList, environment: Environment)(f: LispLambda => LispValue) =
    checkArgsCount(args)(_ == 3)(args match {
      case ConsLispList(name: LispSymbol, ConsLispList(params, ConsLispList(body, _))) =>
        whenValid(lambda(params :: body :: EmptyLispList, environment)) {
          case lambda: LispLambda => environment.whenNotdefined(name) {
            def ldef: LispDef = LispDef(environment + (name, function))

            def function = f(lambda.updated(environment = ldef.environment))

            whenValid(ldef)(_ => ldef)
          }
        }
      case ConsLispList(name, _) => Errors.invalidType(LispTypeStrings.Symbol, name)
    })
  
  def read(args: LispList) = withStringArg(args, NilEnvironment)(str => 
    Utils.read(expression, str)(identity))
  
  def eval(args: LispList, environment: Environment) = withStringArg(args, environment)(evalExpression(_, environment))
  
  def load(args: LispList, environment: Environment) = withStringArg(args, environment) { path =>
    import java.nio.file.{Paths, Files}
    
    // We need to include the input source and the filepath of every expressions.
    // We need them for the tracing of errors later on. The first string represents the parser input,
    // the second one would be the filepath.
    type LispListWithSources = List[(LispValue, String, String)]
    type LoadResult = (LispListWithSources, Option[LispError])
    
    def preload(path: String, pathsIncluded: List[String]): LoadResult = {
      if (Files.exists(Paths.get(path))) {
        def parseError(f: Option[String] => LispError) = (Nil, Some(f(Some(path))))
        
        def recurseRead(input: LispParser.Input, acc: LispListWithSources): LoadResult =
          if (input.atEnd) (acc, None)
          else parse(expression, input) match {
            case success @ Success(expr, next) => expr match {
              // If a "load" expression is found, perform a recursive expansion  
              case ConsLispList(LispSymbol(Keywords.Load), ConsLispList(LispString(path1), EmptyLispList)) => 
                // If a path has already been loaded, skip it. Otherwise, expand it.
                if (pathsIncluded.contains(path1)) recurseRead(next, acc)
                else preload(path1, path :: pathsIncluded) match {
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
    
    preload(path, Nil) match {
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
  
  def head(args: LispList) = withCollArg(args)(_.head)(lstr => LispChar(lstr.value.head))
  
  def tail(args: LispList) = withCollArg(args)(_.tail)(lstr => LispString(lstr.value.tail))
  
  def equal(args: LispList) = checkArgsCount(args)(_ > 0)(args match {
    case llist @ ConsLispList(h, _) => LispBoolean(llist.forAll(_ == h))
  })
  
  def cons(args: LispList) = checkArgsCount(args)(_ == 2)(withCollArg(args.tail)(args.head :: _) { case LispString(str) =>
    args.head match {
      case LispChar(c) => LispString(c + str)
      case lval => 
        val llist = str.foldRight(LispList())((c, llist) => ConsLispList(LispChar(c), llist))
        ConsLispList(lval, llist)
    }
  })
  
  def cond(args: LispList, environment: Environment) = withPairListArgs(args) {
    def recurse(llist: LispList): LispValue = llist match {
      case EmptyLispList => LispNil    // if all conditions yield false, return nil
      case ConsLispList(ConsLispList(condition, ConsLispList(result, _)), tail) =>
        whenValid(Evaluate(condition, environment)) { res =>
          if (res) whenValid(Evaluate(result, environment))(res => res)
          else recurse(tail)
        }
    }
    
    recurse(args)
  }
  
  def quote(args: LispList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, _) => h
  }
  
  def quasiquote(args: LispList, environment: Environment): LispValue = {
    def quasiquote(args: LispList, level: Int): LispValue = args match {
      case EmptyLispList => EmptyLispList
      case ConsLispList(error: LispError, _) => error
      case ConsLispList(atom: LispAtom[_], _) => atom
        
      case ConsLispList(llist: LispList, _) => 
        def recurse(xs: LispList, acc: LispList): LispValue = xs match {
          case EmptyLispList => acc.reverse
          case ConsLispList(error: LispError, _) => error
          case ConsLispList(atom: LispAtom[_], tail) => atom match {
            case LispQuasiQuoteSymbol => whenValid(quasiquote(tail, level + 1))(LispList(LispQuasiQuoteSymbol, _))
            case LispUnquoteSymbol =>
              if (level == 1) tail match {
                case EmptyLispList => EmptyLispList
                case ConsLispList(h, _) => 
                  Evaluate(h, environment)
              } else whenValid(quasiquote(tail, level - 1))(LispList(LispUnquoteSymbol, _))
            case _ => whenValid(atom) { a => recurse(tail, a :: acc) }
          }
          case ConsLispList(EmptyLispList, tail) => recurse(tail, EmptyLispList :: acc)
          case ConsLispList(llist: LispList, tail) => whenValid(recurse(llist, EmptyLispList)) { res =>
            recurse(tail, res :: acc)
          }
        }
        
        recurse(llist, EmptyLispList)
    }
    
    quasiquote(args, 1)
  }
  
  def unquote(args: LispList) = Errors.unquoteNotAllowed 
  
  def isAtom(args: LispList) = checkArgsCount(args)(_ == 1) {
    LispBoolean(args match {
      case EmptyLispList => false
      case ConsLispList(_: LispList, _) => false
      case _ => true
    })
  } 
  
  def isSymbol(args: LispList) = isAtom(args) and LispBoolean(args.head match {
    case LispSymbol(_) | LispDefMacro(_) | _: LispFunction => true
    case _ => false
  })
  
  def isList(args: LispList) = checkArgsCount(args)(_ == 1)(LispBoolean(args match {
    case ConsLispList(_: LispList, _) => true
    case _ => false
  }))
  
  def list(args: LispList) = args
  
  def error(args: LispList) = checkArgsCount(args)(_ == 1)(args match {
    case ConsLispList(LispString(error), _) => LispError(error)
    case lval => Errors.invalidType(LispTypeStrings.String, lval)
  })
  
  def getType(args: LispList): LispValue = checkArgsCount(args)(_ == 1)(args match {
    case ConsLispList(expr, _) => expr match {
      case _: LispList => LispTypeStrings.List
      case _: LispChar => LispTypeStrings.Character
      case LispNil => LispTypeStrings.Nil
      case _: LispFunction => LispTypeStrings.Function
      case _: LispDefMacro => LispTypeStrings.DefMacro
      case _: LispDouble => LispTypeStrings.Double
      case _: LispFloat => LispTypeStrings.Float
      case _: LispInt => LispTypeStrings.Integer
      case _: LispLong => LispTypeStrings.Long
      case _: LispString => LispTypeStrings.String
      case _: LispSymbol => LispTypeStrings.Symbol
      case error: LispError => error
      case lval => Errors.unrecognizedType(lval)
    }
  }) 
}
