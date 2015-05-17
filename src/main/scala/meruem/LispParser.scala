package meruem

import scala.util.parsing.combinator.RegexParsers
import meruem.Constants._

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends RegexParsers {
  def number: Parser[LispNumber] = """-?[0-9]+""".r ^^ (x => LispNumber(x.toLong))
  
  def symbol: Parser[LispSymbol] = """[a-zA-Z0-9_+\-\*\/=<>!@#\$%\^&*\|\?\.]+""".r ^^ (sym => LispSymbol(sym))
  
  def character: Parser[LispChar] = """\\.""".r ^^ (c => LispChar(c.tail.head))
  
  def string: Parser[LispString] = "\"".r ~ """([^\\"]|\\.)*""".r ~ "\"".r ^^ { case _ ~ str ~ _ => LispString(str) }
  
  def quote: Parser[LispValue] = "'" ~ expression ^^ { case _ ~ expr => LispList(LispQuoteSymbol, expr) }
  
  def quasiquote: Parser[LispValue] = "`" ~ expression ^^ { case _ ~ expr => LispList(LispQuasiQuoteSymbol, expr) } 
  
  def unquote: Parser[LispValue] = "," ~ expression ^^ { case _ ~ expr => LispList(LispUnquoteSymbol, expr) }
  
  def comment: Parser[String] = """;[^\r\n]*""".r
  
  def list: Parser[LispList] = "(" ~ rep(expression) ~ ")" ^^ { case _ ~ expr ~ _ => 
    expr.foldLeft(LispList())((acc, h) => h :: acc).reverse
  }
  
  def expression: Parser[LispValue] = (number | symbol | character | quote | quasiquote | unquote | string | list) ^^ {
    case lval: LispValue => lval
  } 
  
  def meruem: Parser[List[LispValue]] = rep(expression)
} 