package meruem.builtins

import meruem._
import meruem.Implicits._
import Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Arithmetics {
  def withNumericArgs[A](args: LispList, initialValue: LispNumber[A])
                        (comp: (LispNumber[Any], LispNumber[A]) => Any)
                        (implicit env: Environment): LispValue = {
    def recurse(valueList: LispList, acc: LispNumber[Any]): LispValue =
      valueList match {
        case NilLispList => acc
        case ConsLispList(h: LispNumber[A], t) => recurse(t, comp(acc, h))
        case ConsLispList(h, _) => Errors.invalidType(LispTypeStrings.Number, h)
      }

    recurse(args, initialValue)
  }
  
  def add(args: LispList, env: Environment) = withNumericArgs(args, 0)(_ + _)(env)
  
  def multiply(args: LispList, env: Environment) = withNumericArgs(args, 1)(_ * _)(env)
  
  def subtract(args: LispList, env: Environment) = decOp(args)(-_)(_ - _)(env)

  def divide(args: LispList, env: Environment): LispValue = decOp(args)(1 / _)(_ / _)(env)
  
  def modulus(args: LispList, env: Environment) = 
    decOp(args)(_ => Errors.incorrectArgCount(1, args)(env))(_ % _)(env)
  
  def decOp(args: LispList)
           (f: LispNumber[Any] => LispValue)
           (g: (LispNumber[Any], LispNumber[Any]) => Any)
           (implicit env: Environment): LispValue = args match {
    case NilLispList => Errors.incorrectArgCount(0, args)
    case ConsLispList(x: LispNumber[_], NilLispList) => f(x)
    case ConsLispList(x: LispNumber[_], tail) => withNumericArgs(tail, x)(g)
    case ConsLispList(x: LispNumber[_], _) => Errors.invalidType(LispTypeStrings.Number, x)
  }
}
