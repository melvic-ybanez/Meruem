package meruem

/**
 * Created by ybamelcash on 5/22/2015.
 */
object Implicits {
  implicit def stringToLispSymbol(str: String): LispSymbol = LispSymbol(str)
  
  implicit def lispValueToBool(lval: LispValue): Boolean = lval.isTrue
  
  implicit def booleanToLispValue(bool: Boolean): LispBoolean = LispBoolean(bool)
  
  implicit def errorToString(error: LispError): String = error.value
  
  implicit def lispNumberToValue[A](lnum: LispNumber[A]) = lnum.value
  
  implicit def intToLispNumber(x: Int): LispInt = LispInt(x)
  
  implicit def longToLispNumber(x: Long): LispLong = LispLong(x)
  
  implicit def floatToLispNumber(x: Float): LispFloat = LispFloat(x)
  
  implicit def doubleToLispNumber(x: Double): LispDouble = LispDouble(x)
  
  implicit def anyToLispNumber(a: Any): LispNumber[Any] = a match {
    case x: Int => x
    case x: Long => x
    case x: Float => x
    case x: Double => x
    case lval => Errors.Exceptions.invalidNumberType(lval)
  }

  implicit def listToLispList[A <: LispValue](exprs: List[A]): LispList = 
    exprs.foldLeft(LispList())((acc, h) => h !: acc).reverse
  
  implicit def lispListToList[A <: LispValue](llist: LispList): List[A] = {
    def recurse(llist: LispList, acc: List[A]): List[A] = llist match {
      case NilLispList => acc
      case (head: A) !: tail => recurse(tail, head :: acc)
    }
    
    recurse(llist, Nil)
  }
  
  implicit def defmacroToLambda(defmacro: LispDefMacro): LispLambda = defmacro.func
}
