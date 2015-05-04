package meruem

import meruem.builtins.Arithmetics._
import meruem.builtins.Functions._

/**
 * Created by ybamelcash on 5/3/2015.
 */
object Globals {
  lazy val environment = NonEmptyEnvironment(
    Map(
      ("+", LispBuiltinFunction(add)),
      ("-", LispBuiltinFunction(subtract)),
      ("*", LispBuiltinFunction(multiply)),
      ("/", LispBuiltinFunction(divide)),
      ("head", LispBuiltinFunction(head)),
      ("tail", LispBuiltinFunction(tail)),
      ("=", LispBuiltinFunction(equal)),
      ("cons", LispBuiltinFunction(cons)),
      ("cond", LispBuiltinFunction(cond)),
      ("quote", LispBuiltinFunction(quote)),
      ("atom?", LispBuiltinFunction(atom))
    ), 
    EmptyEnvironment
  )
}
