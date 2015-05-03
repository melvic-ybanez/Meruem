package meruem

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by ybamelcash on 5/1/2015.
 */
object LispParser extends RegexParsers {
  def number: Parser[LispNumber] = """-?[0-9]+""".r ^^ (x => LispNumber(x.toLong))
  
  def symbol: Parser[LispSymbol] = """[a-zA-Z_+\-\*\/\\=<>!@#\$%\^&*\|?\.,]+""".r ^^ (sym => LispSymbol(sym))
  
  def character: Parser[LispChar] = """\\.""".r ^^ (c => LispChar(c.tail.head))
  
  def string: Parser[LispList] = "\"".r ~ """([^\\"]|\\.)*""".r ~ "\"".r ^^ { case _ ~ str ~ _ =>  
    str.toList.map(LispChar(_)).foldRight(LispList())(_ :: _) 
  }
  
  def comment: Parser[String] = """;[^\r\n]*""".r ^^ (_.toString)
  
  def list: Parser[LispList] = "(" ~ rep(expression) ~ ")" ^^ { case _ ~ expr ~ _ => 
    expr.foldLeft(LispList())((acc, h) => h :: acc).reverse
  }
  
  def expression: Parser[LispValue] = (number | symbol | character | string | comment | list) ^^ {
    case lval: LispValue => lval
  } 
  
  def meruem: Parser[List[LispValue]] = "^".r ~ rep(expression) ~ "$".r ^^ { case _ ~ exprs ~ _ => exprs }
} 