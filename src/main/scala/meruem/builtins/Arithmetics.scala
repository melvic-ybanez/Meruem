package meruem.builtins

import meruem._
import meruem.Implicits._
import meruem.Constants._
import Constants.LispTypeStrings

/**
 * Created by ybamelcash on 4/28/2015.
 */
object Arithmetics {
  def withNumericArgs[A](args: LispList, initialValue: LispNumber[A])
                     (comp: (LispNumber[Any], LispNumber[A]) => Any): LispValue = { 
    def recurse(valueList: LispList, acc: LispNumber[Any]): LispValue = 
      valueList match {
        case EmptyLispList => acc
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
    case EmptyLispList => Errors.incorrectArgCount(0)
    case ConsLispList(x: LispNumber[_], EmptyLispList) => f(x)
    case ConsLispList(x: LispNumber[_], tail) => withNumericArgs(tail, x)(g)
    case ConsLispList(x: LispNumber[_], _) => Errors.invalidType(LispTypeStrings.Number, x)
  }
  
  def compute[A, B](lnum1: LispNumber[A])
                   (lnum2: LispNumber[B])
                   (f: (Int, Int) => Int)
                   (g: (Long, Long) => Long)
                   (h: (Float, Float) => Float)
                   (k: (Double, Double) => Double): Any = (lnum1.value, lnum2.value) match {
    case (x: Int, y: Int) => f(x, y)
    case (x: Int, y: Long) => g(x, y)
    case (x: Long, y: Int) => g(x, y)
    case (x: Long, y: Long) => g(x, y)
    case (x: Int, y: Float) => h(x, y)
    case (x: Float, y: Int) => h(x, y)
    case (x: Long, y: Float) => h(x, y)
    case (x: Float, y: Long) => h(x, y)
    case (x: Float, y: Float) => h(x, y)
    case (x: Int, y: Double) => k(x, y)
    case (x: Double, y: Int) => k(x, y)
    case (x: Long, y: Double) => k(x, y)
    case (x: Double, y: Long) => k(x, y)
    case (x: Float, y: Double) => k(x, y)
    case (x: Double, y: Float) => k(x, y)
    case (x: Double, y: Double) => k(x, y)
    case _ => Errors.Exceptions.invalidNumberType
  }
}
