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
  
  def cons(args: LispList) = checkArgsCount(args)(_ == 2)(whenValid(args.head) { h =>
    isListArg(args.tail)(h :: _)
  })
  
  def cond(args: LispList) = checkArgsCount(args)(_ > 0) {
    args.find {
      case ConsLispList(_, ConsLispList(_, _)) => false
      case  _ => true
    } map (_ => Errors.invalidFormat("Cond accepts only a list of pairs.")) getOrElse {
      def recurse(llist: LispList): LispValue = llist match {
        case EmptyLispList => LispError("All predicates return false")
        case ConsLispList(ConsLispList(EmptyLispList, _), t) => recurse(t)
        case ConsLispList(ConsLispList(_, result), _) => result 
      }
      
      recurse(args)
    }
  }
  
  def quote(args: LispList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, t) => h
  }
  
  def atom(args: LispValue) = args match {
    case _: LispList => LispBoolean(false)
    case _ => LispBoolean(true)
  }
  
  def list(args: LispValue) = args
}
