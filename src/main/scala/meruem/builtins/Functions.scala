package meruem.builtins

import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def head(args: LispList) = isListArg(args)(_.head)
  
  def tail(args: LispList) = isListArg(args)(_.tail)
  
  def equal(args: LispList) = checkArgsCount(args)(_ > 0)(args match {
    case llist @ ConsLispList(h, _) => LispBoolean(llist.forAll(_ == h))
  })
  
  def cons(args: LispList) = checkArgsCount(args)(_ == 2)(validated(args.head) { h =>
    isListArg(args.tail)(h :: _)
  })
  
  def cond(args: LispList, environment: Environment) = checkArgsCount(args)(_ > 0) {
    args.find {   // all arguments must be pairs
      case ConsLispList(_, ConsLispList(_, EmptyLispList)) => false
      case  _ => true
    } map (_ => Errors.invalidFormat("Cond accepts only a list of pairs.")) getOrElse {
      def recurse(llist: LispList): LispValue = llist match {
        case EmptyLispList => LispNil   // if all conditions yield false, return nil
        case ConsLispList(ConsLispList(condition, ConsLispList(result, _)), tail) => 
          validated(Evaluate(condition, environment)) {
            case LispBoolean(false) | LispNil => recurse(tail)
            case _ => validated(Evaluate(result, environment))(_ => result)
          }
      }
      
      recurse(args)
    }
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
}
