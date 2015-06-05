package meruem.builtins

import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 6/5/2015.
 */
object StdIO {
  def readLine(args: LispList, env: Environment) = checkArgsCount(args)(_ == 0) {
    val input = io.StdIn.readLine
    LispString(input)
  } (env)
  
  def print(args: LispList, env: Environment) = withSingleArg(args) { lval =>
    Console.print(args.head)
    LispNil
  } (env)
}
