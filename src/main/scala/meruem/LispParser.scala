package meruem

import scala.util.parsing.combinator._
import meruem.Constants._
import meruem.Implicits._

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends JavaTokenParsers with Modular {
  def integer: Parser[LispInt] = tracked(wholeNumber ^^ (x => LispInt(x.toInt)))
  
  def long: Parser[LispLong] = tracked(wholeNumber <~ Suffixes.LongRegex.r ^^ (x => LispLong(x.toLong)))
  
  def float: Parser[LispFloat] = tracked(
    ((wholeNumber <~ Suffixes.FloatRegex.r) |
    (doublePointRegexString.r <~ Suffixes.FloatRegex.r)) ^^
    (x => LispFloat(x.toFloat)))
  
  def double: Parser[LispDouble] = tracked(
    ((wholeNumber <~ Suffixes.DoubleRegex.r) | 
    (doublePointRegexString.r <~ (Suffixes.DoubleRegex + "?").r)) ^^
    (x => LispDouble(x.toDouble)))
  
  def symbol: Parser[LispSymbol] = tracked("""[a-zA-Z0-9_+\-\*\/=<>!@#\$%\^&*\|\?\.]+""".r ^^ (sym => LispSymbol(sym)))
  
  def character: Parser[LispChar] = tracked("""\\.""".r ^^ (c => LispChar(c.tail.head)))
  
  def string: Parser[LispString] = tracked(stringLiteral ^^ { case str => LispString(str.tail.init) })
  
  def quote: Parser[LispValue] = tracked("'" ~> expression ^^ { case expr => LispList(LispQuoteSymbol, expr) })
  
  def quasiquote: Parser[LispValue] = tracked("`" ~> expression ^^ { case expr => LispList(LispQuasiQuoteSymbol, expr) }) 
  
  def unquote: Parser[LispValue] = tracked("," ~> expression ^^ { case expr => LispList(LispUnquoteSymbol, expr) })
  
  def list: Parser[LispList] = tracked(OpenParen ~> rep(expression) <~ CloseParen ^^ (x => x))
  
  def expression: Parser[LispValue] = tracked((float | double | long | integer | symbol | character | string 
      | quote | quasiquote | unquote | list) ^^ {
    case lval: LispValue => lval
  })
  
  def meruem: Parser[LispList] = tracked(rep(expression) ^^ (x => x))
  
  def tracked[A <: Modular](parser: Parser[A]) = {
    val result = positioned(parser)
    result.setModule(module)
    result
  }
  
  lazy val doublePointRegexString = """-?(\d+(\.\d*)|\d*\.\d+)([eE][+-]?\d+)?"""

  object Suffixes {
    lazy final val FloatRegex = "[fD]"
    lazy final val DoubleRegex = "[dD]"
    lazy final val LongRegex = "[lL]"
  }
} 