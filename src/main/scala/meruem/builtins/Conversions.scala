package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem.Utils._
import meruem._

/**
 * Created by ybamelcash on 5/26/2015.
 */
case object Conversions {
  def toNumber[A](args: LispList)
                 (f: Int => A)
                 (g: Long => A)
                 (h: Float => A)
                 (k: Double => A)
                 (l: Char => A)
                 (m: String => A)
                 (implicit env: Environment): LispValue = withSingleArg(args) {
    case lstr @ LispString(str) => try {
      LispNumber(m(str))
    } catch {
      case ex: NumberFormatException => Errors.numberFormat(lstr) 
    }
    case LispChar(c) => LispNumber(l(c))
    case atom: LispAtom[_] => whenNumber(atom)(f)(g)(h)(k)
    case lval => Errors.invalidType(LispTypeStrings.Number, lval)
  }

  def toInt(args: LispList, env: Environment) = toNumber(args)(identity)(_.toInt)(_.toInt)(_.toInt)(_.toInt)(_.toInt)(env)

  def toLong(args: LispList, env: Environment) = toNumber(args)(_.toLong)(identity)(_.toLong)(_.toLong)(_.toLong)(_.toLong)(env)

  def toFloat(args: LispList, env: Environment) = toNumber(args)(_.toFloat)(_.toFloat)(identity)(_.toFloat)(_.toFloat)(_.toFloat)(env)

  def toDouble(args: LispList, env: Environment) = toNumber(args)(_.toDouble)(_.toDouble)(_.toDouble)(identity)(_.toDouble)(_.toDouble)(env)
  
  def toLispString(args: LispList, env: Environment) = withSingleArg(args) {
    case str: LispString => str
    case lval => LispString(lval.toString)
  } (env)
  
  def toChar(args: LispList, env: Environment) = withSingleArg(args) {
    case lchar: LispChar => lchar
    case LispInt(x) => LispChar(x.toChar)
    case LispLong(x) => LispChar(x.toChar)
    case LispFloat(x) => LispChar(x.toChar)
    case LispDouble(x) => LispChar(x.toChar)
    case lval => Errors.invalidType(LispTypeStrings.Number, lval)(env)
  } (env)
}
