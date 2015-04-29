package meruem.builtins

import meruem._
import meruem.Utils.Aliases.LispValueList
import meruem.Utils._

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Functions {
  def head(args: LispValueList) = withListArg(args)(_.head)
  
  def tail(args: LispValueList) = withListArg(args)(_.tail)
  
  def equals(args: LispValueList) = requireArgs(args)(sanitizeAll(args) match {
    case error: LispError => error
    case llist: LispList => LispBoolean(args.tail.forAll(_ == args.head))
  })
}
