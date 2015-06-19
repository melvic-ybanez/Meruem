package meruem.builtins.io

import meruem.Utils._
import meruem._

/**
 * Created by ybamelcash on 6/5/2015.
 */
object Std {
  def readLine(args: LispList, env: Environment) = checkArgsCount(args)(_ == 0) {
    val input = scala.io.StdIn.readLine
    LispString(input)
  } (env)
  
  def print(args: LispList, env: Environment) = withSingleArg(args) { lval =>
    Console.print(args.head)
    LispNil
  } (env)
}
