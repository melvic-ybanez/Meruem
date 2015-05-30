package meruem

import scala.util.parsing.input.Positional

/**
 * Created by ybamelcash on 5/29/2015.
 */

trait Modular extends Positional {
  var module: Module = NilModule
  
  def setModule(newModule: Module) = {
    module = newModule
  }
}
