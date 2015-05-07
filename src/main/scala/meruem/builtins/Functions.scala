package meruem.builtins

import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
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
  
  def cond(args: LispList, environment: Environment) = checkArgsCount(args)(_ > 0) {
    args.find(!isPair(_)).map(lval => Errors.nonPair(lval)) getOrElse {
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
