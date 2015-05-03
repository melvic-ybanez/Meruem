package meruem

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends RegexParsers {
  
  def number: Parser[LispNumber] = """-?[0-9]+""".r ^^ (x => LispNumber(x.toLong))
  
  def symbol: Parser[LispSymbol] = """[^()0-9]""".r ^^ (sym => LispSymbol(sym))
  
  def char: Parser[LispChar] = """\\.""".r ^^ (c => LispChar(c.head))
  
  def string: Parser[LispList] = """"([^\\"]|\\.)*"""".r ^^ {  
    _.map(LispChar(_)).foldLeft(LispList())((acc, h) => h :: acc) 
  }
  
  def comment: Parser[String] = """;[^\r\n]*""".r ^^ (_.toString)
  
  def list: Parser[LispValue] = """\(""" ~ expr ~ """\)""" ^^ { case _ ~ expr ~ _ => expr }
  
  def expr: Parser[LispValue] = (number | symbol | char | string | comment | list) ^^ {
    case lval: LispValue => lval
  } 
}