package meruem.builtins

import meruem._
import Constants.LispTypeStrings
import Utils.Aliases.LispValueList

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Arithmetics {
  def withNumericArgs(args: LispValueList, initialValue: Long)
                     (compute: (Long, Long) => Long): LispValue = {
    def recurse(valueList: LispValueList, acc: Long): LispValue = 
      valueList match {
        case EmptyLispList => LispNumber(acc)
        case ConsLispList(h: LispNumber, t) => recurse(t, compute(acc, h))
        case ConsLispList(h, _) => Errors.invalidType(LispTypeStrings.Number, h)
      }
    
    recurse(args, initialValue)
  }
  
  def add(args: LispValueList) = withNumericArgs(args, 0)(_ + _)
  
  def multiply(args: LispValueList) = withNumericArgs(args, 1)(_ * _)
  
  def subtract(args: LispValueList) = args match {
    case EmptyLispList => Errors.incorrectArgCount(0)
    case ConsLispList(LispNumber(x), _) => LispNumber(-x)
    case ConsLispList(LispNumber(x), tail) => withNumericArgs(tail, x)(_ - _)
  }

  def divide(args: LispValueList) = args match {
    case EmptyLispList => Errors.incorrectArgCount(0)
    case ConsLispList(LispNumber(x), _) => LispNumber(1 / x)
    case ConsLispList(LispNumber(x), tail) => withNumericArgs(tail, x)(_ / _)
  }
}