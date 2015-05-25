package meruem

import scala.util.parsing.combinator._
import meruem.Constants._

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends JavaTokenParsers {
  def integer: Parser[LispInt] = wholeNumber ^^ (x => LispInt(x.toInt))
  
  def long: Parser[LispLong] = wholeNumber <~ Suffixes.LongRegex.r ^^ (x => LispLong(x.toLong))
  
  def float: Parser[LispFloat] = ((wholeNumber <~ Suffixes.FloatRegex.r) |
    (doublePointRegexString.r <~ Suffixes.FloatRegex.r)) ^^
    (x => LispFloat(x.toFloat))
  
  def double: Parser[LispDouble] = ((wholeNumber <~ Suffixes.DoubleRegex.r) | 
    (doublePointRegexString.r <~ (Suffixes.DoubleRegex + "?").r)) ^^
    (x => LispDouble(x.toDouble))
  
  def symbol: Parser[LispSymbol] = """[a-zA-Z0-9_+\-\*\/=<>!@#\$%\^&*\|\?\.]+""".r ^^ (sym => LispSymbol(sym))
  
  def character: Parser[LispChar] = """\\.""".r ^^ (c => LispChar(c.tail.head))
  
  def string: Parser[LispString] = stringLiteral ^^ { case str => LispString(str.tail.init) }
  
  def quote: Parser[LispValue] = "'" ~> expression ^^ { case expr => LispList(LispQuoteSymbol, expr) }
  
  def quasiquote: Parser[LispValue] = "`" ~> expression ^^ { case expr => LispList(LispQuasiQuoteSymbol, expr) } 
  
  def unquote: Parser[LispValue] = "," ~> expression ^^ { case expr => LispList(LispUnquoteSymbol, expr) }
  
  def list: Parser[LispList] = OpenParen ~> rep(expression) <~ CloseParen ^^ (exprsToLispList(_))
  
  def expression: Parser[LispValue] = (float | double | long | integer | symbol | character | string 
      | quote | quasiquote | unquote | list) ^^ {
    case lval: LispValue => lval
  }
  
  def meruem: Parser[LispList] = rep(expression) ^^ (exprsToLispList(_))
  
  lazy val doublePointRegexString = """-?(\d+(\.\d*)|\d*\.\d+)([eE][+-]?\d+)?"""

  object Suffixes {
    lazy final val FloatRegex = "[fD]"
    lazy final val DoubleRegex = "[dD]"
    lazy final val LongRegex = "[lL]"
  }

  final val OpenParen = "("
  final val CloseParen = ")"
  
  def exprsToLispList(exprs: List[LispValue]) = exprs.foldLeft(LispList())((acc, h) => h :: acc).reverse
} 