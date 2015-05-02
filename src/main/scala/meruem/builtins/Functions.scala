package meruem.builtins

import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def head(args: LispList) = hasListArg(args)(_.head)
  
  def tail(args: LispList) = hasListArg(args)(_.tail)
  
  def equals(args: LispList) = checkArgsCount(args)(_ > 0)(sanitizeAll(args) {
    case llist @ ConsLispList(h, _) => LispBoolean(llist.forAll(_ == h))
  })
  
  def cons(args: LispList) = checkArgsCount(args)(_ == 2)(whenValid(args.head.evaluate) { h =>
    hasListArg(args.tail)(h :: _)
  })
  
  def cond(args: LispList) = checkArgsCount(args)(_ > 0) {
    args.find {
      case ConsLispList(_, ConsLispList(_, _)) => true
      case  _ => false
    } map (_ => Errors.invalidFormat("Cond accepts only a list of pairs.")) getOrElse {
      sanitizeAll(args) { llist =>
        def recurse(llist: LispList): LispValue = llist match {
          case EmptyLispList => LispError("All predicates return false")
          case ConsLispList(ConsLispList(EmptyLispList, _), t) => recurse(t)
          case ConsLispList(ConsLispList(_, result), _) => result 
        }
        
        recurse(llist)
      }
    }
  }
  
  def quote(args: LispList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, t) => h
  }
  
  def atom(args: LispValue) = args match {
    case _: LispList => false
    case _ => true
  }
}
