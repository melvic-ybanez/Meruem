package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Constants._
import meruem.Implicits.{anyToLispNumber => _, _}
import meruem._
import meruem.Utils._
import meruem.LispParser._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def defmacro(args: LispList)(implicit environment: Environment) = defineFunction(args)(LispDefMacro(_))
  
  def getMacro(args: LispList, env: Environment) = checkArgsCount(args)(_ == 1) {
    args.head match {
      case lmacro: LispDefMacro => lmacro
      case _ => LispNil
    }
  }(env)

  def lambda(args: LispList)(implicit env: Environment): LispValue = checkArgsCount(args)(_ == 2)(args match {
    case (llist: LispList) !: body !: _ => allSymbols(llist) {
      LispLambda(llist, NilLispList, body, SomeEnvironment(collection.mutable.Map(), env))
    }
    case llist !: _  => Errors.invalidType(LispTypeStrings.List, llist)
  })
  
  def define(args: LispList)(implicit env: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case (sym: LispSymbol) !: value !:  _ => 
      // Check whether the symbol has already been defined or not
      env.whenNotdefined(sym) {
        whenValid(Evaluate(value)) {
          case lval => LispDef(env += (sym, lval))
        }
      }
    case lval !: _ => Errors.invalidType(LispTypeStrings.Symbol, lval)
  })
  
  def defun(args: LispList)(implicit env: Environment) = defineFunction(args)(identity)

  def defineFunction(args: LispList)(f: LispLambda => LispValue)(implicit env: Environment) =
    checkArgsCount(args)(_ == 3)(args match {
      case (name: LispSymbol) !: params !: body !:  _ =>
        whenValid(lambda(params !: body !: NilLispList)) {
          case lambda: LispLambda => env.whenNotdefined(name) {
            whenValid(f(lambda)) { func =>
              LispDef(env += (name, func))
            }
          }
        }
      case name !: _ => Errors.invalidType(LispTypeStrings.Symbol, name)
    })
  
  def read(args: LispList, env: Environment) = withStringArg(args)(str => 
    Utils.read(expression, str)(identity)(env))(env)
  
  def eval(args: LispList)(implicit env: Environment) = withStringArg(args)(evalExpression(_, env))
  
  def head(args: LispList, env: Environment) = withCollArg(args)(_.head)(lstr => LispChar(lstr.value.head))(env)
  
  def tail(args: LispList, env: Environment) = withCollArg(args)(_.tail)(lstr => LispString(lstr.value.tail))(env)
  
  def cons(args: LispList, env: Environment) = {
    implicit val env1 = env
    checkArgsCount(args)(_ == 2)(withCollArg(args.tail)(args.head !: _) { case LispString(str) =>
      args.head match {
        case LispChar(c) => LispString(c.toString + str)
        case lval =>
          val llist = str.foldRight(LispList())((c, llist) => ConsLispList(LispChar(c), llist))
          lval !: llist
      }
    })
  }
  
  def cond(args: LispList)(implicit env: Environment) = withPairListArgs(args) {
    def recurse(llist: LispList): LispValue = llist match {
      case NilLispList => LispNil    // if all conditions yield false, return nil
      case (condition !: result !: _) !: tail =>
        whenValid(Evaluate(condition)(env)) { res =>
          if (res) whenValid(Evaluate(result)(env))(res => res)
          else recurse(tail)
        }
    }
    
    recurse(args)
  }
  
  def quote(args: LispList) = args match {
    case NilLispList => NilLispList
    case ConsLispList(h, _) => h
  }
  
  def quasiquote(args: LispList)(implicit env: Environment): LispValue = {
    def quasiquote(args: LispList, level: Int): LispValue = args match {
      case NilLispList => NilLispList
      case (atom: LispAtom[_]) !: _ => atom
        
      case (llist: LispList) !: _ => 
        def recurse(xs: LispList, acc: LispList): LispValue = xs match {
          case NilLispList => acc.reverse
          case (atom: LispAtom[_]) !: tail => atom match {
            case LispQuasiQuoteSymbol => LispQuasiQuoteSymbol !: quasiquote(tail, level + 1) !: NilLispList
            case LispUnquoteSymbol =>
              if (level == 1) tail match {
                case NilLispList => NilLispList
                case ConsLispList(h, _) => Evaluate(h)
              } else LispUnquoteSymbol !: quasiquote(tail, level - 1) !: NilLispList
            case _ => recurse(tail, atom !: acc)
          }
          case NilLispList !: tail => recurse(tail, NilLispList !: acc)
          case (llist: LispList) !: tail => recurse(tail, recurse(llist, NilLispList) !: acc)
          
        }
        
        recurse(llist, NilLispList)
    }
    
    quasiquote(args, 1)
  }
  
  def unquote(args: LispList)(implicit env: Environment) = Errors.unquoteNotAllowed(args)
  
  def list(args: LispList, env: Environment) = args
  
  def error(args: LispList, env: Environment) = withSingleArg(args) {
    case LispString(error) => LispError(error, args)(env)
    case lval => Errors.invalidType(LispTypeStrings.String, lval)(env)
  } (env)
  
  def getType(args: LispList, env: Environment): LispValue = withSingleArg(args) {
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
    case _: LispBoolean => LispTypeStrings.Boolean
    case error: LispError => error
    case lval => Errors.unrecognizedType(lval)(env)
  } (env)
}
