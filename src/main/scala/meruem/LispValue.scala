package meruem

import meruem.Utils._

import scala.util.parsing.input.Positional

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue {
  def isTrue = this match {
    case LispBoolean(false) | LispNil | LispError(_) => false
    case _ => true
  }
  
  def or(that: => LispValue) = if (this) this else that
  
  def and(that: => LispValue) = if (this) that else this
}

sealed trait LispAtom[+A] extends LispValue {
  def value: A
  
  override def toString = value.toString
}

case class LispNumber(value: Long) extends LispAtom[Long]

case class LispBoolean(value: Boolean) extends LispAtom[Boolean]

case class LispChar(value: Char) extends LispAtom[Char] {
  override def toString = raw"""\$value"""
}

case class LispString(value: String) extends LispAtom[String] {
  override def toString = raw""""$value""""
}

case object LispNil extends LispAtom[Nothing] {
  def value = throw new NullPointerException("reading from nil")
  
  override def toString = "nil"
}

case class LispError(value: String) extends LispValue {
  override def toString = "Error. " + value
}

case class LispSymbol(value: String) extends LispAtom[String]

case class LispDef(environment: Environment) extends LispValue {
  override def toString = LispNil.toString
}

sealed trait LispList extends LispValue {
  def head: LispValue
  def tail: LispList
  
  def ::(lval: LispValue): LispList = this match {
    case EmptyLispList => ConsLispList(lval, EmptyLispList)
    case _: ConsLispList => ConsLispList(lval, this) 
  }
  
  def ++(llist: LispList): LispList = 
    reverse.foldLeft(llist)((acc, h) => h :: acc)
  
  def find(p: LispValue => Boolean): Option[LispValue] = {
    def recurse(llist: LispList): Option[LispValue] = llist match {
      case EmptyLispList => None
      case ConsLispList(h, t) => 
        if (p(h)) Some(h) else recurse(t)
    }
    
    recurse(this)
  } 
  
  def size: Int = this match {
    case EmptyLispList => 0
    case ConsLispList(h, t) => 1 + t.size
  }

  def exists(p: LispValue => Boolean) = find(p).nonEmpty
  
  def forAll(p: LispValue => Boolean) = !exists(!p(_))
  
  def filter(p: LispValue => Boolean): LispList = foldLeft(LispList()) { (acc, h) =>
    if (p(h)) h :: acc else acc
  }

  def map(f: LispValue => LispValue): LispList = {
    def recurse(llist: LispList, acc: LispList): LispList = llist match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, f(h) :: acc)
    }
    
    recurse(this, EmptyLispList).reverse
  }
  
  def foldLeft[A <: LispValue](initialValue: A)(f: (A, LispValue) => A) = {
    def recurse(llist: LispList, acc: A): A = llist match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, f(acc, h))
    }
    
    recurse(this, initialValue)
  }
  
  def reverse: LispList = foldLeft(LispList())((acc, h) => h :: acc)
  
  override def toString = {
    def recurse(xs: LispList, acc: List[String]): List[String] = xs match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, h.toString :: acc)
    } 
    
    val str = recurse(this, Nil).reverse.mkString(" ")
    Constants.ListOpenParen + str + Constants.ListCloseParen  
  }
}

case object EmptyLispList extends LispList {
  def head = throw new IllegalAccessException("Empty lisp list has no head.")
  
  def tail = throw new IllegalAccessException("Empty lisp list has no tail.")
}

case class ConsLispList(head: LispValue, tail: LispList) extends LispList 

object LispList {
  def apply(lval: LispValue*): LispList = 
    if (lval.isEmpty) EmptyLispList
    else lval.foldLeft[LispList](EmptyLispList)((llist, h) => h :: llist).reverse
}

sealed trait LispFunction extends LispValue {
  def environment: Environment
  
  override def toString = "<function>"
}

case class LispBuiltinFunction(func: LispList => LispValue) extends LispFunction {
  
  def environment: Environment = NilEnvironment
}

class LispLambda(val params: LispList, 
                         val args: LispList,
                         val body: LispValue,
                         environ: => Environment) extends LispFunction {
  lazy val environment = environ
  
  def updated(params: LispList = params,
              args: LispList = args,
              body: LispValue = body,
              environment: => Environment = environment): LispLambda = 
    new LispLambda(params, args, body, environment)
}

case object LispLambda {
  def apply(params: LispList, args: LispList, body: LispValue, environment: => Environment) =
    new LispLambda(params, args, body, environment)
  
  def unapply(lambda: LispLambda) = Some(lambda.params, lambda.args, lambda.body, lambda.environment)
}

case class LispDefMacro(func: LispLambda) extends LispValue

