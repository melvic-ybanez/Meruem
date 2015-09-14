package meruem

import java.io.File

import meruem.Constants._
import meruem.Implicits._
import meruem.LispParser._

import scala.util.parsing.input.CharArrayReader

/**
 * Created by ybamelcash on 4/27/2015.
 */

object Utils {
  def read[A <: LispValue](expression: LispParser.Parser[A], input: String)
                          (f: A => LispValue)
                          (implicit env: Environment): LispValue = {
    LispParser.parseAll(expression, new CharArrayReader(input.toArray)) match {
      case Success(expr, _) => f(expr)
      case failure@Failure(msg, next) =>  Errors.parseFailure(msg, next.pos) 
      case error@Error(msg, next) => Errors.parseError(msg, next.pos)
    }
  }
  
  def evalExpression(str: String, environment: Environment): LispValue = 
    read(expression, str)(Evaluate(_)(environment))(environment)

  def macroExpand(lval: LispValue)(implicit env: Environment): LispValue = lval match {
    case LispDefMacro(func) !: tail =>
      macroExpand(Evaluate(func.updated(args = tail.map(LispList(QuoteSymbol, _)))))
    case _ => lval
  }
  
  def whenValid[A <: LispValue, B <: LispValue](lval: A)(f: A => B) = lval match {
    case error: LispError => error
    case lval: A => f(lval)
  }

  def checkArgsCount(args: LispList)(p: Int => Boolean)(f: => LispValue)(implicit env: Environment) =
    if (!p(args.size)) Errors.incorrectArgCount(args.size, args) else f

  def sanitizeAll(args: LispList)(f: (LispList, Environment) => LispValue)(implicit env: Environment) =  
    whenValid(args.foldLeft[LispValue](NilLispList) { (acc, lval) =>
      whenValid(acc) {
        case llist: LispList => whenValid(Evaluate(lval))(_ !: llist)
      }
    }) {
      case lval: LispList => f(lval.reverse, env)
      case error: LispError => error
      case lval => Errors.invalidType(LispTypeStrings.List, lval)
    }
  
  def withCollArg(args: LispList)(f: LispList => LispValue)(g: LispString => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ == 1) {
      args.head match {
        case llist: LispList => f(llist)
        case str: LispString => g(str)
        case lval => Errors.invalidType(LispTypeStrings.List, lval)
      }
    }
  
  def isPair(expr: LispValue) = expr match {
    case _ !: _ !: NilLispList => true
    case _ => false
  }
  
  def withPairListArgs(args: LispList)(f: => LispValue)(implicit env: Environment) = checkArgsCount(args)(_ > 0) {
    args.find(!isPair(_)).map(expr => Errors.invalidType(LispTypeStrings.Pair, expr)) getOrElse f 
  }
  
  def allSymbols(llist: LispList)(f: => LispValue)(implicit env: Environment) = llist.find {
    case _: LispSymbol => false
    case _ => true
  } map(lval => Errors.invalidType(LispTypeStrings.Symbol, lval)) getOrElse f
  
  def withStringArg(args: LispList)(f: String => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ == 1) {
      whenValid(Evaluate(args.head)) {
        case LispString(str) => f(str)
        case lval => Errors.invalidType(LispTypeStrings.String, lval)
      }
    }
  
  def whenNumber[A](lval: LispValue)
                   (f: Int => A)
                   (g: Long => A)
                   (h: Float => A)
                   (k: Double => A)
                   (implicit env: Environment): LispValue = lval match {
    case LispInt(x) => f(x)
    case LispLong(x) => g(x)
    case LispFloat(x) => h(x)
    case LispDouble(x) => k(x)
    case _ => Errors.invalidType(LispTypeStrings.Integer, lval)
  }
  
  def withSingleArg(args: LispList)(f: LispValue => LispValue)(implicit env: Environment): LispValue = 
    checkArgsCount(args)(_ == 1)(args match {
      case expr !: _ => f(expr)
    })
  
  def withPairArgs(args: LispList)
                  (f: (LispValue, LispValue) => LispValue)
                  (implicit env: Environment): LispValue = checkArgsCount(args)(_ == 2)(args match {
    case expr1 !: expr2 !: _ => f(expr1, expr2) 
  }) 
  
  def withAtLeastOneArg(args: LispList)(f: LispList => LispValue)(implicit env: Environment) = 
    checkArgsCount(args)(_ > 0)(f(args))

  def compute[A, B, C, D, E, F](lnum1: LispNumber[A])
                               (lnum2: LispNumber[B])
                               (f: (Int, Int) => C)
                               (g: (Long, Long) => D)
                               (h: (Float, Float) => E)
                               (k: (Double, Double) => F): Any = (lnum1.value, lnum2.value) match {
    case (x: Int, y: Int) => f(x, y)
    case (x: Int, y: Long) => g(x, y)
    case (x: Long, y: Int) => g(x, y)
    case (x: Long, y: Long) => g(x, y)
    case (x: Int, y: Float) => h(x, y)
    case (x: Float, y: Int) => h(x, y)
    case (x: Long, y: Float) => h(x, y)
    case (x: Float, y: Long) => h(x, y)
    case (x: Float, y: Float) => h(x, y)
    case (x: Int, y: Double) => k(x, y)
    case (x: Double, y: Int) => k(x, y)
    case (x: Long, y: Double) => k(x, y)
    case (x: Double, y: Long) => k(x, y)
    case (x: Float, y: Double) => k(x, y)
    case (x: Double, y: Float) => k(x, y)
    case (x: Double, y: Double) => k(x, y)
  }
  
  def isDefineCommand(str: String) = List(Keywords.Def, Keywords.DefMacro) contains str

  /** Converts the list of tuples (of names and values) into two separate lists.
    *
    * @return A LispList of two LispLists. The first list contains the names, 
    *         and the second list contains the values. If there's at least one 
    *         value that evalutes to an error, this function returns that error.
    */
  def unzip(args: LispList)(implicit env: Environment) = {
    def unzip(xs: LispList, acc: (LispList, LispList)): LispValue = xs match {
      case NilLispList => LispList(acc._1.reverse, acc._2.reverse)
      case ((name: LispSymbol) !: value !: _) !: tail => acc._1.find {
        _ == name
      } map { _ =>
        Errors.alreadyDefined(name)
      } getOrElse unzip(tail, (name !: acc._1, value !: acc._2))
      case (lval !: _) !: _ => Errors.invalidType(LispTypeStrings.Symbol, lval)
    }
    
    unzip(args, (NilLispList, NilLispList))
  }

  def filePathToModulePath(pathString: String) = {
    val filePaths = pathString.split(s"""\\${File.separator}""")
    val modulePath = filePaths.mkString(ModuleSeparator)
    modulePath
  }
}
