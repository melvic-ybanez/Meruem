/**
 * Created by ybamelcash on 4/27/2015.
 */

import java.io.EOFException

import meruem.{Utils, Globals, Evaluate}
import meruem.LispParser._

import io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    // READ-EVAL-PRINT-LOOP
    while (true) {
      val input = readLine("meruem>")
      println(Utils.read(input))
    }  
  }
}
