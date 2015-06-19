package meruem.builtins

import java.util.UUID

import meruem.Constants.{LispTypeStrings, _}
import meruem.Implicits.{anyToLispNumber => _, lispListToList => _, _}
import meruem.Utils._
import meruem._
import meruem.LispParser._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def defmacro(args: LispList)(implicit env: Environment) = checkArgsCount(args)(_ == 3)(args match {
    case (name: LispSymbol) !: params !: body !:  _ =>
      whenValid(lambda(params !: body !: NilLispList)) {
        case llambda: LispLambda => env.whenNotdefined(name) {
          LispDef(env += (name, LispDefMacro(llambda)))
        }
      }
    case name !: _ => Errors.invalidType(LispTypeStrings.Symbol, name)
  })
  
  def getMacro(args: LispList, env: Environment) = withSingleArg(args) {
    case lmacro: LispDefMacro => lmacro 
    case _ => LispNil
  } (env)
  
  def gensym(args: LispList, env: Environment) = checkArgsCount(args)(_ == 0) {
    val uuid = UUID.randomUUID.toString
    LispSymbol(s"G_$uuid")
  } (env)

  def lambda(args: LispList)(implicit env: Environment): LispValue = withPairArgs(args) {
    case (llist: LispList, body) => allSymbols(llist) {
      LispLambda(llist, NilLispList, body, SomeEnvironment(collection.mutable.Map(), env))
    }
    case (llist, _)  => Errors.invalidType(LispTypeStrings.List, llist)
  }
  
  def define(args: LispList)(implicit env: Environment) = withPairArgs(args) { 
    case (sym: LispSymbol, value) => 
      // Check whether the symbol has already been defined or not
      env.whenNotdefined(sym) {
        whenValid(Evaluate(value)) {
          case lval => LispDef(env += (sym, lval))
        }
      }
    case (lval, _) => Errors.invalidType(LispTypeStrings.Symbol, lval)
  } 
  
  def read(args: LispList, env: Environment) = withStringArg(args)(str => 
    Utils.read(expression, str)(identity)(env))(env)

  def eval(args: LispList, env: Environment) = withSingleArg(args)(Evaluate(_)(env))(env)
  
  def applyFunc(args: LispList)(implicit env: Environment) = withPairArgs(args) { (fn, fargs) =>
    whenValid(Evaluate(fargs)) {
      case xs: LispList => Evaluate(fn !: xs)
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  }
  
  def letExpression(args: LispList)(implicit env: Environment) = withPairArgs(args) {
    case (xs: LispList, body) => withPairListArgs(xs) {
      whenValid(unzip(xs)) {
        case (names: LispList) !: (values: LispList) !: _ => whenValid(lambda(LispList(names, body))) {
          case function: LispLambda => Evaluate(function.updated(args = values))
        }
      }
    }
    case (lval, _) => Errors.invalidType(LispTypeStrings.List, lval)
  }
  
  def tailRec(args: LispList)(implicit env: Environment): LispValue = withPairArgs(args) {
    case (args: LispList, expr) => withPairListArgs(args) {
      whenValid(unzip(args)) { case (params: LispList) !: (inits: LispList) !: _ =>
        @annotation.tailrec
        def recurse(args: LispList): LispValue = lambda(LispList(params, expr)) match {
          case error: LispError => error
          case llambda: LispLambda =>
            Evaluate(llambda.updated(args = args)) match {
              case LispRecur(resultArgs) =>
                // The resultArgs are the result of calling the recur function. It is assumed that
                // the items have already been evaluated (recur is just a normal function, after all).
                // We need to quote them to prevent double evaluations.
                val quotedArgs = resultArgs.map(LispList(QuoteSymbol, _))
                
                recurse(quotedArgs)
              case lval => lval
            }
        }
        
        recurse(inits)  
      }
    }
    case (lval, _) => Errors.invalidType(LispTypeStrings.List, lval)
  }
  
  def recur(args: LispList, env: Environment) = LispRecur(args)
  
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
  
  def cond(args: LispList)(implicit env: Environment) = args match {
    case NilLispList => LispNil
    case _ => withPairListArgs(args) {
      def recurse(llist: LispList): LispValue = llist match {
        case NilLispList => LispNil // if all conditions yield false, return nil
        case (condition !: result !: _) !: tail =>
          whenValid(Evaluate(condition)(env)) { res =>
            if (res) whenValid(Evaluate(result)(env))(res => res)
            else recurse(tail)
          }
      }

      recurse(args)
    }
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
            case QuasiQuoteSymbol => QuasiQuoteSymbol !: quasiquote(tail, level + 1) !: NilLispList
            case UnquoteSymbol =>
              if (level == 1) tail match {
                case NilLispList => NilLispList
                case ConsLispList(h, _) => Evaluate(h)
              } else UnquoteSymbol !: quasiquote(tail, level - 1) !: NilLispList
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
    case LispString(msg) => LispError(msg, args)(env)
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
  
  def tryCatch(args: LispList)(implicit env: Environment): LispValue = checkArgsCount(args)(size => size == 2 || size == 3) {
    args match {
      case lval !: catchExpr !: NilLispList => tryCatch(lval !: catchExpr !: LispNil !: NilLispList)
      case lval !: catchExpr !: finallyExpr !: _ => whenValid(Evaluate(catchExpr)) {
        case LispLambda(params, _, body, environ) => try {
          Evaluate(lval)
        } catch {
          case ex: Exception =>
            val position = LispInt(lval.pos.line) !: LispInt(lval.pos.column) !: NilLispList
            val quotedPosition = QuoteSymbol !: position !: NilLispList
            val args = LispString(ex.toString) !: quotedPosition !: LispString(lval.pos.longString) !: NilLispList
            Evaluate(LispLambda(params, args, body, environ))
        } finally {
          Evaluate(finallyExpr)
        }
        case lval1 => Errors.invalidType(LispTypeStrings.Function, lval1)
      }
    }
  } 
}
