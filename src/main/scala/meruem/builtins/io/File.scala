package meruem.builtins.io

import java.io.BufferedReader
import java.nio.file.{Path, Paths, Files}
import java.nio.charset.Charset
import java.util.stream.Collectors
import scala.collection.JavaConverters._

import meruem.Constants.LispTypeStrings
import meruem._
import meruem.Utils._
import meruem.Implicits.listToLispList

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

  def readLines(args: LispList, env: Environment): LispValue = checkArgsCount(args)(size => size == 3 || size == 4)(args match {
    // If a charset is not provided, use the default one  
    case expr1 !: expr2 !: expr3 !: NilLispList => 
      readLines(expr1 !: expr2 !: expr3 !: LispString(Charset.defaultCharset.toString) !: NilLispList, env)
      
    case LispString(strPath) !: (lambda: LispLambda) !: defaultValue !: LispString(charset) !: _ => 
      val path = Paths.get(strPath)
      val readFile = managed(Files.newBufferedReader(path, Charset.forName(charset))) map { reader =>
        def recurse(acc: LispValue): LispValue = Option(reader.readLine) map { line =>
          Evaluate(lambda.updated(args = LispList(acc, LispString(line))))(env) match {
            case error: LispError => error
            case LispNil => acc
            case result => recurse(result)
          }
        } getOrElse acc
        
        recurse(defaultValue)
      }
      readFile.opt.getOrElse(LispError("Unable to read file: " + strPath, args)(env))
      
    // Ensure that the 1st, 2nd and 4th types are all correct  
    case LispString(_) !: (_: LispLambda) !: _ !: lval !: _ => Errors.invalidType(LispTypeStrings.String, lval)(env)
    case LispString(_) !: lval !: _ => Errors.invalidType(LispTypeStrings.Function, lval)(env)
    case lval !: _ => Errors.invalidType(LispTypeStrings.String, lval)(env)
  })(env)
  
  def write(args: LispList, env: Environment): LispValue = checkArgsCount(args)(size => size == 2 || size == 3) {
    def error[A <: LispValue](lval: A) = Errors.invalidType(LispTypeStrings.String, lval)(env)
    
    args match {
      case expr1 !: expr2 !: NilLispList =>
        write(expr1 !: expr2 !: LispString(Charset.defaultCharset.toString) !: NilLispList, env)
      case LispString(filePath) !: LispString(content) !: LispString(charset) !: _ =>
        val path = Paths.get(filePath)
        val writeToFile = managed(Files.newBufferedWriter(path, Charset.forName(charset))) map { writer =>
          writer.write(content)
          LispNil
        }
        writeToFile.opt.getOrElse(LispError("Unable to write to file: " + filePath, args)(env))
      case LispString(_) !: LispString(_) !: lval !: _ => error(lval)
      case LispString(_) !: lval !: _ => error(lval) 
      case lval !: _ => error(lval)
    }
  } (env)
  
  def listFiles(args: LispList, env: Environment) = withFile(args) { path => 
    Files.newDirectoryStream(path).asScala.map(path => LispString(path.toString)).toList
  } (env)
  
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
