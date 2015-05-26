package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem._
import meruem.Utils._
import meruem.Implicits._

/**
 * Created by ybamelcash on 5/26/2015.
 */
object Relational {
  def equal(args: LispList) = withAtLeastOneArg(args) {
    case llist @ ConsLispList(h, _) => LispBoolean(llist.forAll(_ == h))
  }
  
  def not(args: LispList) = withSingleArg(args) { expr => LispBoolean(if (expr) false else true) }
  
  def > (args: LispList) = relationalOp(args)(_ > _)
  
  def < (args: LispList) = relationalOp(args)(_ < _)
  
  def relationalOp(args: LispList)
                  (f: (LispNumber[Any], LispNumber[Any]) => Any) = withAtLeastOneArg(args) { 
    case llist @ ConsLispList(h: LispNumber[_], t) =>
      def recurse[A](llist: LispList, greatest: LispNumber[A]): LispValue = llist match {
        case EmptyLispList => LispBoolean(true)
        case ConsLispList(x: LispNumber[A], tail) =>  
          val flag = f(greatest, x) match { case bool: Boolean => LispBoolean(bool) }
          if (flag) recurse(tail, x) else LispBoolean(false)
        case ConsLispList(x, _) => Errors.invalidType(LispTypeStrings.Number, x)
      }
      
      recurse(t, h)
    case ConsLispList(h, _) => Errors.invalidType(LispTypeStrings.Number, h)
  }
}
