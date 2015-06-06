package meruem.builtins

import java.nio.file.Paths

import meruem.Constants.LispTypeStrings
import meruem.{LispInt, LispString, LispList, Environment}
import meruem.Utils._
import meruem._

/**
 * Created by ybamelcash on 6/6/2015.
 */
object FileIO {
  def toPath(args: LispList, env: Environment) = withPath(args)(identity)(env)
  
  def pathFileName(args: LispList, env: Environment) = withPath(args)(_.getFileName)(env)
  
  def pathName(args: LispList, env: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case LispInt(x) !: rest => withPath(rest)(_.getName(x))(env)
    case lval !: _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
  })(env)
  
  def pathNameCount(args: LispList, env: Environment) = withStringArg(args) {
    path => LispInt(Paths.get(path).getNameCount)
  } (env)
  
  def subpath(args: LispList, env: Environment) = checkArgsCount(args)(_ == 3)(args match {
    case LispInt(x) !: LispInt(y) !: rest => withPath(rest)(_.subpath(x, y))(env)
    case lval !:  _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
    case LispInt(_) !: lval !: _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
  })(env)
  
  def getParent(args: LispList, env: Environment) = withPath(args)(_.getParent)(env)
  
  def getRoot(args: LispList, env: Environment) = withPath(args)(_.getRoot)(env)
}
