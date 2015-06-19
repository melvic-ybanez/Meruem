package meruem.builtins

import meruem.Constants.LispTypeStrings
import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 6/19/2015.
 */
object Math {
  def sin(args: LispList, env: Environment) = withDoubleArg(args)(math.sin)(env)
  
  def cos(args: LispList, env: Environment) = withDoubleArg(args)(math.cos)(env)
  
  def tan(args: LispList, env: Environment) = withDoubleArg(args)(math.tan)(env)

  def ceil(args: LispList, env: Environment) = withDoubleArg(args)(math.ceil)(env)

  def exp(args: LispList, env: Environment) = withDoubleArg(args)(math.exp)(env)

  def floor(args: LispList, env: Environment) = withDoubleArg(args)(math.floor)(env)

  def log(args: LispList, env: Environment) = withDoubleArg(args)(math.log)(env)

  def log10(args: LispList, env: Environment) = withDoubleArg(args)(math.log10)(env)
  
  def random(args: LispList, env: Environment) = checkArgsCount(args)(_ == 0)(LispDouble(math.random))(env)
  
  def sqrt(args: LispList, env: Environment) = withDoubleArg(args)(math.sqrt)(env)
  
  def withDoubleArg(args: LispList)(f: Double => Double)(implicit env: Environment) = withSingleArg(args) {
    case LispInt(x) => LispDouble(f(x))
    case LispLong(x) => LispDouble(f(x))
    case LispFloat(x) => LispDouble(f(x))
    case LispDouble(x) => LispDouble(f(x))
    case lval => Errors.invalidType(LispTypeStrings.Double, lval)
  }
}
