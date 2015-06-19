package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Implicits._
import meruem.Utils._
import meruem._

/**
 * Created by ybamelcash on 5/26/2015.
 */
object Relational {
  def equal(args: LispList, env: Environment) = withAtLeastOneArg(args) {
    case llist @ ConsLispList(h, _) => LispBoolean(llist.forAll(_ == h))
  }(env)
  
  def not(args: LispList, env: Environment) = withSingleArg(args) { expr => LispBoolean(if (expr) false else true) }(env)
  
  def > (args: LispList, env: Environment) = relationalOp(args)(_ > _)(env)
  
  def < (args: LispList, env: Environment) = relationalOp(args)(_ < _)(env)
  
  def relationalOp(args: LispList)
                  (f: (LispNumber[Any], LispNumber[Any]) => Any)
                  (implicit env: Environment) = withAtLeastOneArg(args) { 
    case llist @ ConsLispList(h: LispNumber[_], t) =>
      def recurse[A](llist: LispList, greatest: LispNumber[A]): LispValue = llist match {
        case NilLispList => LispBoolean(true)
        case ConsLispList(x: LispNumber[A], tail) =>  
          val flag = f(greatest, x) match { case bool: Boolean => LispBoolean(bool) }
          if (flag) recurse(tail, x) else LispBoolean(false)
        case ConsLispList(x, _) => Errors.invalidType(LispTypeStrings.Number, x)(env)
      }
      
      recurse(t, h)
    case ConsLispList(h, _) => Errors.invalidType(LispTypeStrings.Number, h)(env)
  }
}
