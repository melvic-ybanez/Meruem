package meruem

/**
 * Created by ybamelcash on 5/22/2015.
 */
object Implicits {
  implicit def stringToLispString(str: String): LispString = LispString(str)
  
  implicit def lispValueToBool(lval: LispValue): Boolean = lval.isTrue
  
  implicit def errorToString(error: LispError): String = error.value
  
  implicit def lispNumberToInt[A](lnum: LispNumber[A]) = lnum.value
  
  implicit def intToLispNumber(x: Int): LispInt = LispInt(x)
  
  implicit def longToLispNumber(x: Long): LispLong = LispLong(x)
  
  implicit def floatToLispNumber(x: Float): LispFloat = LispFloat(x)
  
  implicit def doubleToLispNumber(x: Double): LispDouble = LispDouble(x)
  
  implicit def anyToLispNumber(a: Any): LispNumber[Any] = a match {
    case x: Int => x
    case x: Long => x
    case x: Float => x
    case x: Double => x
    case _ => Errors.invalidNumberType
  }
}
