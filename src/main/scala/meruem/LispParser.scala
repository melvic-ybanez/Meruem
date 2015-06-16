package meruem

import scala.util.parsing.combinator._
import meruem.Constants._
import meruem.Implicits.listToLispList

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends JavaTokenParsers {
  def integer: Parser[LispInt] = positioned(wholeNumber ^^ (x => LispInt(x.toInt)))
  
  def long: Parser[LispLong] = positioned(wholeNumber <~ Suffixes.LongRegex.r ^^ (x => LispLong(x.toLong)))
  
  def float: Parser[LispFloat] = positioned(
    ((wholeNumber <~ Suffixes.FloatRegex.r) |
    (doublePointRegexString.r <~ Suffixes.FloatRegex.r)) ^^
    (x => LispFloat(x.toFloat)))
  
  def double: Parser[LispDouble] = positioned(
    ((wholeNumber <~ Suffixes.DoubleRegex.r) | 
    (doublePointRegexString.r <~ (Suffixes.DoubleRegex + "?").r)) ^^
    (x => LispDouble(x.toDouble)))
  
  def boolean: Parser[LispBoolean] = positioned(s"""${Keywords.True}|${Keywords.False}""".r ^^ (bool => LispBoolean(bool.toBoolean)))
  
  def nil: Parser[LispAtom[Nothing]] = positioned(Keywords.LNil.r ^^ (_ => LispNil))
  
  def symbol: Parser[LispSymbol] = positioned("""[a-zA-Z0-9_+\-\*\/=<>!@:#\$%\^&*\|\?\.]+""".r ^^ (sym => LispSymbol(sym)))
  
  def character: Parser[LispChar] = positioned("""\\.""".r ^^ (c => LispChar(c.tail.head)))
  
  def string: Parser[LispString] = positioned(stringLiteral ^^ { case str => LispString(str.tail.init) })
  
  def quote: Parser[LispValue] = positioned("'" ~> expression ^^ { case expr => LispList(QuoteSymbol, expr) })
  
  def quasiquote: Parser[LispValue] = positioned("`" ~> expression ^^ { case expr => LispList(QuasiQuoteSymbol, expr) }) 
  
  def unquote: Parser[LispValue] = positioned("," ~> expression ^^ { case expr => LispList(UnquoteSymbol, expr) })
  
  def list: Parser[LispList] = positioned(OpenParen ~> rep(expression) <~ CloseParen ^^ (x => x))
  
  def expression: Parser[LispValue] = positioned((float | double | long | integer | boolean | nil | 
      symbol | character | string | quote | quasiquote | unquote | list) ^^ {
    case lval: LispValue => lval
  })
  
  def meruem: Parser[LispList] = positioned(rep(expression) ^^ (x => x))
  
  lazy val doublePointRegexString = """-?(\d+(\.\d*)|\d*\.\d+)([eE][+-]?\d+)?"""

  object Suffixes {
    lazy final val FloatRegex = "@[fF]"
    lazy final val DoubleRegex = "@[dD]"
    lazy final val LongRegex = "@[lL]"
  }
} 