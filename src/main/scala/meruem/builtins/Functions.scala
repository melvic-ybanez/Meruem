package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem._
import meruem.Utils._
import meruem.LispParser._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  /*def defun(args: LispList) = checkArgsCount(args)(_ == 3)(args match {
    case ConsLispList(name: LispSymbol, params: LispList, ConsLispList(body, _)) => 
  })*/
  
  def lambda(args: LispList, environment: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case ConsLispList(llist: LispList, ConsLispList(body, _)) => llist.find {
      case _: LispSymbol => false
      case _ => true
    } map(lval => Errors.invalidType(LispTypeStrings.Symbol, lval)) getOrElse {
      LispCustomFunction(llist, EmptyLispList, body, NonEmptyEnvironment(Map(), environment))
    }  
  })

  def define(args: LispList, environment: Environment) = withPairListArgs(args) {
    def recurse(llist: LispList, result: LispDef): LispValue = llist match {
      case EmptyLispList => result
      case ConsLispList(ConsLispList(sym: LispSymbol, ConsLispList(value, _)), tail) =>
        // Check whether the symbol has already been defined or not
        if (!environment.hasSymbol(sym))
          whenValid(Evaluate(value, environment)) {
            case lval => recurse(tail, LispDef(result.environment + (sym, lval)))
          }
        else Errors.alreadyDefined(sym)
      case ConsLispList(ConsLispList(lval, _), _) => Errors.invalidType(LispTypeStrings.Symbol, lval)
    }

    recurse(args, LispDef(environment))
  }
  
  def read(args: LispList, environment: Environment) = checkArgsCount(args)(_ == 1)(args.head match {
    case LispString(str) => Utils.read(str, environment) 
    case lval => Errors.invalidType(LispTypeStrings.String, lval)
  }) 
  
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
        whenValid(Evaluate(condition, environment)) {
          case LispBoolean(false) | LispNil => recurse(tail)
          case _ => whenValid(Evaluate(result, environment))(_ => result)
        }
    }
    
    recurse(args)
  }
  
  def quote(args: LispList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, t) => h
  }
  
  def atom(args: LispList) = checkArgsCount(args)(_ == 1) {
    args match {
      case EmptyLispList => LispBoolean(false)
      case ConsLispList(_: LispList, t) => LispBoolean(false)
      case _ => LispBoolean(true)
    }
  }
  
  def list(args: LispList) = args
  
  def error(args: LispList) = checkArgsCount(args)(_ == 1)(args match {
    case ConsLispList(LispString(error), _) => LispError(error)
    case lval => Errors.invalidType(LispTypeStrings.String, lval)
  })
}
