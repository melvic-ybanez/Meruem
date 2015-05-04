package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue

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

case class LispError(value: String) extends LispValue {
  override def toString = "Error: " + value
}

case class LispSymbol(value: String) extends LispAtom[String]

case object LispQuote extends LispValue

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
  def args: LispList
  def environment: Environment
  
  override def toString = "<function>"
}

case class LispBuiltinFunction(func: LispList => LispValue,
                               args: LispList = EmptyLispList,
                               environment: Environment = EmptyEnvironment) extends LispFunction {
  
  def updated(args: LispList = args,
              func: LispList => LispValue = func,
              environment: Environment = environment) =
    LispBuiltinFunction(func, args, environment)
}

case class LispCustomFunction(params: LispList, 
                              args: LispList,
                              body: LispValue,
                              environment: Environment) extends LispFunction {
  def updated(params: LispList = params,
              args: LispList = args,
              body: LispValue = body,
              environment: Environment = environment): LispValue = 
    LispCustomFunction(params, args, body, environment)
}

