package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Constants._
import meruem._
import meruem.Utils._
import meruem.LispParser._

import scala.io.Source

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
  
  def read(args: LispList) = withStringArg(args, NilEnvironment)(str => Utils.read(str, expression)(identity))
  
  def eval(args: LispList, environment: Environment) = withStringArg(args, environment)(evalExpression(_, environment))
  
  def load(args: LispList, environment: Environment) = withStringArg(args, environment) { path =>
    import java.nio.file.{Paths, Files}
    
    def preLoad(path: String, pathsIncluded: List[String]): LispValue = {
      if (Files.exists(Paths.get(path))) 
        whenValid(Utils.read(Source.fromFile(path).mkString, meruem)(identity)) {
          case exprs: LispList =>
            def expandLoads(xs: LispList, acc: LispList): LispValue = xs match {
              case EmptyLispList => acc
              
              // If a "load" expression is found, do recursive expandsion  
              case ConsLispList(ConsLispList(LispSymbol(Keywords.Load), ConsLispList(LispString(path1), EmptyLispList)), t) =>
                // If a path has already been loaded, skip it, otherwise expand it.
                if (pathsIncluded.contains(path1)) expandLoads(t, acc)
                else whenValid(preLoad(path1, path :: pathsIncluded)) { case loadedExprs: LispList =>
                  expandLoads(t, loadedExprs)
                }
                
              case ConsLispList(expr, tail) => expandLoads(tail, expr :: acc)
            }
            
            expandLoads(exprs, EmptyLispList)
          case lval => lval  
        }
      else Errors.fileNotFound(path)
    }
    
    whenValid(preLoad(path, Nil)) {
      case exprs: LispList =>
        // Check if there are function and macro declarations 
        // (i.e list expressions that start with either "defun" or "defmacro")
        def recurse(exprs: LispList,
                    environment: Environment,
                    functions: LispList): (LispValue, Environment) = exprs match {
          case EmptyLispList => (functions, environment)
          case ConsLispList(expr, tail) => expr match {
            case llist @ ConsLispList(LispSymbol(sym), _)
              if sym == Keywords.Defun || sym == Keywords.DefMacro =>
              recurse(tail, environment, llist :: functions)
            case _ => Evaluate(expr, environment) match {
              case error: LispError => (error, environment)
              case LispDef(envi) => recurse(tail, envi, functions)
              case lval => recurse(tail, environment, functions)
            }
          }
        }

        val (result, updatedEnvi) = recurse(exprs, environment, EmptyLispList)

        whenValid(result) {
          case functions: LispList =>
            // Remember: LispDef evaluates its constructor arguments lazily.
            def ldef: LispDef = LispDef(newEnvironment)

            // Evaluate each of the functions and macros found and register each of them to the environment.
            // We need to update only the valueMaps at first, then pass it as an argument to the new environment.
            // The purpose is so that we wouldn't have to create a new environment everytime we register a new function
            // or macro, which is dangerous since an environment is immutable and we might end up having our functions
            // and macros point to different environments. So, the map gets constructed first, then we pass it to a single
            // environment, then have all those functions and macros point to that environment. 
            lazy val (newEnvironment, errorOpt) = {
              def recurse(functions: LispList,
                          values: Map[String, LispValue]): (Map[String, LispValue], Option[LispError]) =
                functions match {
                  case EmptyLispList => (values, None)
                  case ConsLispList(llist: LispList, _) if llist.size != 4 =>
                    (values, Some(Errors.incorrectArgCount(llist.size - 1)))
                  case ConsLispList(ConsLispList(LispSymbol(op),
                  ConsLispList(nameSym @ LispSymbol(name), ConsLispList(params, ConsLispList(body, _)))), tail) =>
                    lambda(params :: body :: EmptyLispList, environment) match {
                      case llambda: LispLambda =>
                        if (values.exists(_._1 == name)) (values, Some(Errors.alreadyDefined(nameSym)))
                        else recurse(tail, values + (name -> {
                          val llambda1 = llambda.updated(environment = ldef.environment)
                          if (op == "defun") llambda1 else LispDefMacro(llambda1)
                        }))
                      case error: LispError => (values, Some(error))
                    }
                  case ConsLispList(ConsLispList(_, ConsLispList(name, _)), _) =>
                    (values, Some(Errors.invalidType(LispTypeStrings.Symbol, name)))
                }

              val (valueMap, errorOpt) = recurse(functions, updatedEnvi.valueMap)
              (SomeEnvironment(valueMap, updatedEnvi.parent), errorOpt)
            }

            errorOpt.getOrElse(ldef)

          case lval => LispDef(updatedEnvi)
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
}
