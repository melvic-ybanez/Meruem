package meruem.builtins.io

import java.nio.file.{Path, Paths}

import meruem.Constants.LispTypeStrings
import meruem.Utils._
import meruem.{Environment, LispInt, LispList, _}

/**
 * Created by ybamelcash on 6/6/2015.
 */
object Path {
  def toPath(args: LispList, env: Environment) = withPath(args)(identity)(env)
  
  def pathFileName(args: LispList, env: Environment) = withPath(args)(_.getFileName)(env)
  
  def pathName(args: LispList, env: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case LispInt(x) !: rest => withPath(rest)(_.getName(x))(env)
    case lval !: _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
  })(env)
  
  def pathNameCount(args: LispList, env: Environment) = withStringArg(args) { path => 
    LispInt(Paths.get(path).getNameCount)
  } (env)
  
  def subpath(args: LispList, env: Environment) = checkArgsCount(args)(_ == 3)(args match {
    case LispInt(x) !: LispInt(y) !: rest => withPath(rest)(_.subpath(x, y))(env)
    case LispInt(_) !: lval !: _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
    case lval !:  _ => Errors.invalidType(LispTypeStrings.Integer, lval)(env)
  })(env)
  
  def getParent(args: LispList, env: Environment) = withPath(args)(_.getParent)(env)
  
  def getRoot(args: LispList, env: Environment) = withPath(args)(_.getRoot)(env)
  
  def normalize(args: LispList, env: Environment) = withPath(args)(_.normalize)(env)
  
  def toURI(args: LispList, env: Environment) = withPath(args)(path => Paths.get(path.toUri.toString))(env)
  
  def toAbsolutePath(args: LispList, env: Environment) = withPath(args)(_.toAbsolutePath)(env)
  
  def toRealPath(args: LispList, env: Environment) = withPath(args)(_.toRealPath())(env)
  
  def resolve(args: LispList, env: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case LispString(str) !: rest => withPath(rest)(_.resolve(str))(env)
    case lval !: _ => Errors.invalidType(LispTypeStrings.String, lval)(env)
  })(env)
  
  def relativize(args: LispList, env: Environment) = checkArgsCount(args)(_ == 2)(args match {
    case LispString(str1) !: LispString(str2) !: _ => 
      val path1 = Paths.get(str1)
      val path2 = Paths.get(str2)
      LispString(path1.relativize(path2).toString)
    case LispString(_) !: lval !: _ => Errors.invalidType(LispTypeStrings.String, lval)(env)
    case lval !: _ => Errors.invalidType(LispTypeStrings.String, lval)(env)
  })(env)

  def withPath(args: LispList)(f: Path => Path)(implicit env: Environment) =
    withStringArg(args)(path => LispString(f(Paths.get(path)).toString))
}
