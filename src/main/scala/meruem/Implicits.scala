package meruem

/**
 * Created by ybamelcash on 5/22/2015.
 */
object Implicits {
  implicit def lispValueToBool(lval: LispValue): Boolean = lval.isTrue
  
  implicit def errorToString(error: LispError): String = error.value
}
