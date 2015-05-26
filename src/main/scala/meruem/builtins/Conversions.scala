package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Utils._
import meruem._

/**
 * Created by ybamelcash on 5/26/2015.
 */
object Conversions {
  def toNumber[A](args: LispList)
                 (f: Int => A)
                 (g: Long => A)
                 (h: Float => A)
                 (k: Double => A): LispValue = checkArgsCount(args)(_ == 1)(args match {
    case ConsLispList(atom: LispAtom[_], _) => whenNumber(atom)(f)(g)(h)(k)
    case lval => Errors.invalidType(LispTypeStrings.Number, lval)
  })

  def toInt(args: LispList) = toNumber(args)(identity)(_.toInt)(_.toInt)(_.toInt)

  def toLong(args: LispList) = toNumber(args)(_.toLong)(identity)(_.toLong)(_.toLong)

  def toFloat(args: LispList) = toNumber(args)(_.toFloat)(_.toFloat)(identity)(_.toFloat)

  def toDouble(args: LispList) = toNumber(args)(_.toDouble)(_.toDouble)(_.toDouble)(identity)
}
