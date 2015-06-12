package meruem.builtins.io

import java.nio.file.{Path, Paths, Files}

import meruem.Constants.LispTypeStrings
import meruem._
import meruem.Utils._

import resource._

/**
 * Created by ybamelcash on 6/8/2015.
 */
object File {
  def exists(args: LispList, env: Environment) = filePredicate(args)(Files.exists(_))(env)
  
  def isReadable(args: LispList, env: Environment) = filePredicate(args)(Files.isReadable)(env)
  
  def isWriteable(args: LispList, env: Environment) = filePredicate(args)(Files.isWritable)(env)
  
  def isExecuatable(args: LispList, env: Environment) = filePredicate(args)(Files.isExecutable)(env)
  
  def delete(args: LispList, env: Environment) = withFile(args) { path =>
    Files.delete(path)
    LispNil
  } (env)
  
  def copy(args: LispList, env: Environment) = withPathPairArgs(args)((path1, path2) =>
    LispString(Files.copy(path1, path2).toString))(env)
  
  def size(args: LispList, env: Environment) = withFile(args)(path => LispLong(Files.size(path)))(env)
  
  def isDirectory(args: LispList, env: Environment) = filePredicate(args)(Files.isDirectory(_))(env)
  
  def isHidden(args: LispList, env: Environment) = filePredicate(args)(Files.isHidden)(env)

  
  
  def withFile(args: LispList)(f: Path => LispValue)(implicit env: Environment) = withStringArg(args)(path =>
    f(Paths.get(path)))
  
  def filePredicate(args: LispList)(f: Path => Boolean)(implicit env: Environment) = withFile(args)(path =>
    LispBoolean(f(path)))
  
  def withPathPairArgs(args: LispList)
                      (f: (Path, Path) => LispValue)
                      (implicit env: Environment) = withPairArgs(args) {
    case (LispString(path1), LispString(path2)) => f(Paths.get(path1), Paths.get(path2))
    case (LispString(_), lval) => Errors.invalidType(LispTypeStrings.String, lval)
    case (lval, _) => Errors.invalidType(LispTypeStrings.String, lval)
  }
}
