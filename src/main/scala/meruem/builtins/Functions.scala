package meruem.builtins

import meruem._
import meruem.Utils.Aliases.LispValueList
import meruem.Utils._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def head(args: LispValueList) = isListArg(args)(_.head)
  
  def tail(args: LispValueList) = isListArg(args)(_.tail)
  
  def equals(args: LispValueList) = checkArgsCount(args)(_ > 0)(sanitizeAll(args) {
    case ConsLispList(h, t) => LispBoolean(t.forAll(_ == h))
  })
  
  def cons(args: LispValueList) = checkArgsCount(args)(_ == 2)(whenValid(args.head.evaluate) { h =>
    isListArg(args.tail)(h :: _)
  })
  
  def cond(args: LispValueList) = checkArgsCount(args)(_ > 0) {
    args.find {
      case ConsLispList(_, ConsLispList(_, _)) => true
      case  _ => false
    } map (_ => Errors.invalidFormat("Cond accepts only a list of pairs.")) getOrElse {
      sanitizeAll(args) { llist =>
        def recurse(llist: LispValueList): LispValue = llist match {
          case EmptyLispList => LispError("All predicates return false")
          case ConsLispList(ConsLispList(EmptyLispList, _), t) => recurse(t)
          case ConsLispList(ConsLispList(_, t), _) => t 
        }
        
        recurse(llist)
      }
    }
  }
  
  def quote(args: LispValueList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, t) => h
  }
}
