/**
 * Created by ybamelcash on 4/27/2015.
 */

import java.io.EOFException

import meruem.LispParser._

import io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    while (true) {
      val input = readLine("meruem>")
      
      parse(meruem, input) match {
        case Success(Nil, _) => println("Error: EOF while reading")
        case Success(lval, _) => println(lval.mkString)
        case Failure(msg, _) => println("Failure: " + msg)
        case Error(msg, _) => println("Error: " + msg)
      }
    }  
  }
}
