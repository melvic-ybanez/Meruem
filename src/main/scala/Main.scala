/**
 * Created by ybamelcash on 4/27/2015.
 */

import java.io.EOFException

import meruem._
import meruem.LispParser._

import io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    // READ-EVAL-PRINT-LOOP
    def repl(environment: Environment) {
      val input = readLine("meruem>")
      Utils.evalExpression(input, environment) match {
        case ldef @ LispDef(newEnvironment) => 
          println(ldef)
          repl(newEnvironment)
        case lval => 
          println(lval)
          repl(environment)
      } 
    }  
    
    repl(Globals.environment)
  }
}
