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
  
  def equals(args: LispValueList) = requireArgs(args)(sanitizeAll(args) match {
    case error: LispError => error
    case llist: LispList => LispBoolean(args.tail.forAll(_ == args.head))
  })
  
  def cons(args: LispValueList) = checkArgsCount(args, 2)(whenValid(args.head.evaluate) { h =>
    isListArg(args.tail)(h :: _)
  })
  
  def quote(args: LispValueList) = args match {
    case EmptyLispList => EmptyLispList
    case ConsLispList(h, t) => h
  }
}
