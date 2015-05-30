package meruem.builtins

import meruem._
import meruem.Implicits._
import Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Arithmetics {
  def withNumericArgs[A](args: LispList, initialValue: LispNumber[A])
                        (comp: (LispNumber[Any], LispNumber[A]) => Any): LispValue = {
    def recurse(valueList: LispList, acc: LispNumber[Any]): LispValue =
      valueList match {
        case NilLispList => acc
        case ConsLispList(h: LispNumber[A], t) => recurse(t, comp(acc, h))
        case ConsLispList(h, _) => Errors.invalidType(LispTypeStrings.Number, h)
      }

    recurse(args, initialValue)
  }
  
  def add(args: LispList) = withNumericArgs(args, 0)(_ + _)
  
  def multiply(args: LispList) = withNumericArgs(args, 1)(_ * _)
  
  def subtract(args: LispList) = decOp(args)(-_)(_ - _)

  def divide(args: LispList): LispValue = decOp(args)(1 / _)(_ / _)
  
  def modulus(args: LispList) = decOp(args)(_ => Errors.incorrectArgCount(1))(_ % _)
  
  def decOp(args: LispList)
           (f: LispNumber[Any] => LispValue)
           (g: (LispNumber[Any], LispNumber[Any]) => Any): LispValue = args match {
    case NilLispList => Errors.incorrectArgCount(0)
    case ConsLispList(x: LispNumber[_], NilLispList) => f(x)
    case ConsLispList(x: LispNumber[_], tail) => withNumericArgs(tail, x)(g)
    case ConsLispList(x: LispNumber[_], _) => Errors.invalidType(LispTypeStrings.Number, x)
  }
}
