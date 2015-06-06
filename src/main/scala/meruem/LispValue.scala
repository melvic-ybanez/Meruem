package meruem

import meruem.Implicits._
import meruem.Utils._
import meruem.Constants._

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue extends Positional {
  def isTrue = this match {
    case LispBoolean(false) | LispNil => false
    case _ => true
  }
  
  def or(that: => LispValue) = if (this) this else that
  
  def and(that: => LispValue) = if (this) that else this
}

sealed trait LispAtom[+A] extends LispValue {
  def value: A
  
  override def toString = value.toString
}

trait LispNumber[+A] extends LispAtom[A] {
  lazy val computeThis = compute(this) _
  
  def + [B](that: LispNumber[B]) = computeThis(that)(_ + _)(_ + _)(_ + _)(_ + _)
  
  def - [B](that: LispNumber[B]) = computeThis(that)(_ - _)(_ - _)(_ - _)(_ - _)
  
  def * [B](that: LispNumber[B]) = computeThis(that)(_ * _)(_ * _)(_ * _)(_ * _)

  def / [B](that: LispNumber[B]) = computeThis(that)(_ / _)(_ / _)(_ / _)(_ / _)

  def % [B](that: LispNumber[B]) = computeThis(that)(_ % _)(_ % _)(_ % _)(_ % _)
  
  def > [B](that: LispNumber[B]) = computeThis(that)(_ > _)(_ > _)(_ > _)(_ > _)
  
  def < [B](that: LispNumber[B]) = computeThis(that)(_ < _)(_ < _)(_ < _)(_ < _)
  
  def unary_- : LispNumber[Any] = LispInt(0) - this
}

case class LispInt(value: Int) extends LispNumber[Int] 

case class LispLong(value: Long) extends LispNumber[Long]

case class LispFloat(value: Float) extends LispNumber[Float]

case class LispDouble(value: Double) extends LispNumber[Double]

object LispNumber {
  def apply(value: Any): LispNumber[Any] = value match {
    case x: Int => LispInt(x)
    case x: Long => LispLong(x)
    case x: Float => LispFloat(x)
    case x: Double => LispDouble(x)
  }
}

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

case class LispError(value: String, lval: LispValue)(implicit environment: Environment) extends LispValue {
  override def toString = 
    s"An error has occurred. $value\n" +
    "Source: " + (environment.module match {
      case NilModule => Globals.module.filePath
      case SomeModule(path, _, _) => path 
    }) + s" [${lval.pos.line}:${lval.pos.column}}]\n" +
    lval.pos.longString
}

case class LispSymbol(value: String) extends LispAtom[String]

case class LispDef(environment: Environment) extends LispValue {
  override def toString = LispNil.toString
}

sealed trait LispList extends LispValue {
  def head: LispValue
  def tail: LispList
  
  def !:(lval: LispValue): LispList = this match {
    case NilLispList => ConsLispList(lval, NilLispList)
    case _: ConsLispList => ConsLispList(lval, this) 
  }
  
  def ++(llist: LispList): LispList = 
    reverse.foldLeft(llist)((acc, h) => h !: acc)
  
  def find(p: LispValue => Boolean): Option[LispValue] = {
    def recurse(llist: LispList): Option[LispValue] = llist match {
      case NilLispList => None
      case ConsLispList(h, t) => 
        if (p(h)) Some(h) else recurse(t)
    }
    
    recurse(this)
  } 
  
  def size: Int = this match {
    case NilLispList => 0
    case ConsLispList(h, t) => 1 + t.size
  }

  def exists(p: LispValue => Boolean) = find(p).nonEmpty
  
  def forAll(p: LispValue => Boolean) = !exists(!p(_))
  
  def filter(p: LispValue => Boolean): LispList = foldLeft(LispList()) { (acc, h) =>
    if (p(h)) h !: acc else acc
  }

  def map(f: LispValue => LispValue): LispList = {
    def recurse(llist: LispList, acc: LispList): LispList = llist match {
      case NilLispList => acc
      case ConsLispList(h, t) => recurse(t, f(h) !: acc)
    }
    
    recurse(this, NilLispList).reverse
  }
  
  def foldLeft[A <: LispValue](initialValue: A)(f: (A, LispValue) => A) = {
    def recurse(llist: LispList, acc: A): A = llist match {
      case NilLispList => acc
      case ConsLispList(h, t) => recurse(t, f(acc, h))
    }
    
    recurse(this, initialValue)
  }
  
  def reverse: LispList = foldLeft(LispList())((acc, h) => h !: acc)
  
  override def toString = {
    def recurse(xs: LispList, acc: List[String]): List[String] = xs match {
      case NilLispList => acc
      case ConsLispList(h, t) => recurse(t, h.toString :: acc)
    } 
    
    val str = recurse(this, Nil).reverse.mkString(" ")
    Constants.OpenParen + str + Constants.CloseParen  
  }
}

case object NilLispList extends LispList {
  def head = throw new IllegalAccessException("Empty lisp list has no head.")
  
  def tail = throw new IllegalAccessException("Empty lisp list has no tail.")
}

case class ConsLispList(head: LispValue, tail: LispList) extends LispList {
  setPos(head.pos)
}

object LispList {
  def apply(lval: LispValue*): LispList = 
    if (lval.isEmpty) NilLispList
    else lval.foldLeft[LispList](NilLispList)((llist, h) => h !: llist).reverse
}

object !: {
  def unapply(llist: ConsLispList) = Some(llist.head, llist.tail)
}

sealed trait LispFunction extends LispValue {
  def environment: Environment
  
  override def toString = "<function>"
}

case class LispBuiltinFunction(func: (LispList, Environment) => LispValue) extends LispFunction {
  def environment: Environment = NilEnvironment
}

case class LispLambda(params: LispList,
                 args: LispList,
                 body: LispValue,
                 environment: Environment) extends LispFunction {
  def updated(params: LispList = params,
              args: LispList = args,
              body: LispValue = body,
              environment: Environment = environment): LispLambda = 
    LispLambda(params, args, body, environment)
}

case class LispDefMacro(func: LispLambda) extends LispValue

import scala.collection.mutable.MutableList

trait Module extends LispValue {
  def filePath: String
  def submodules: MutableList[Module]
  def environment: Environment
}

case object NilModule extends Module {
  def filePath = throwError("filePath")
  
  def environment = throwError("environment")
  
  def submodules = throwError("submodules")

  def throwError(memberName: String) =
    throw new IllegalAccessException(s"""Can not access member "$memberName" of nil module""")
}

/** This class represents an existing module.
  * 
  * This isn't a case class due to the cyclic reference with the environment param. While there would have been
  * no problem with the construction since the submodules needed by the environment is mutable (if this was a
  * case class), the printing of this class would throw a stackoverflow error. For that reason, we needed to make 
  * this a non-case-class and then manually override the toString method.
  */
class SomeModule(val filePath: String, val submodules: MutableList[Module], val environment: Environment) extends Module {
  override def toString = s"SomeModule($filePath, ${submodules.map(_.filePath)}, ${environment.valueMap.map(_._1)}})"
}

object SomeModule {
  def apply(filePath: String, submodules: MutableList[Module], env: Environment) = 
    new SomeModule(filePath, submodules, env)
  
  def unapply(module: SomeModule) = Some(module.filePath, module.submodules, module.environment)
}