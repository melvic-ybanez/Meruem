package meruem

import meruem.Utils._

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue {
  def evaluate: LispValue
}

sealed trait LispAtom[+A] extends LispValue {
  def value: A
  
  def evaluate = this
  
  override def toString = value.toString
}

case class LispNumber(value: Long) extends LispAtom[Long]

case class LispBoolean(value: Boolean) extends LispAtom[Boolean]

case class LispChar(value: Char) extends LispAtom[Char] {
  override def toString = raw"""\$value"""
}

case class LispError(value: String) extends LispValue {
  def evaluate = this
  
  override def toString = "Error: " + value
}

case class LispSymbol(value: String) extends LispAtom[String] {
  override def evaluate = ??? 
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
  
  def evaluate = this
}

case class ConsLispList(head: LispValue, tail: LispList) extends LispList {
  def evaluate = head.evaluate match {
    case builtinFunc: LispBuiltinFunction => builtinFunc.updated(args = tail).evaluate
    case customFunc: LispCustomFunction => customFunc.updated(args = tail).evaluate
    case lval => Errors.nonFunction(lval) 
  }
}

object LispList {
  def apply(lval: LispValue*): LispList = 
    if (lval.isEmpty) EmptyLispList
    else lval.foldLeft[LispList](EmptyLispList)((llist, h) => h :: llist)
}

sealed trait LispFunction extends LispValue {
  def symbol: LispSymbol
  def args: LispList
  def environment: Environment
  
  override def toString = s"<function ${symbol.value}>"
}

case class LispBuiltinFunction(symbol: LispSymbol, 
                               args: LispList, 
                               func: LispList => LispValue,
                               environment: Environment) extends LispFunction {
  def evaluate = func(args)
  
  def updated(symbol: LispSymbol = symbol,
               args: LispList = args,
               func: LispList => LispValue = func,
               environment: Environment = environment) =
    LispBuiltinFunction(symbol, args, func, environment)
}

case class LispCustomFunction(symbol: LispSymbol, 
                              params: LispList, 
                              args: LispList,
                              body: LispValue,
                              environment: Environment) extends LispFunction {
  def evaluate = sanitizeAll(args) {
    case EmptyLispList => params match {
      // If each of the arguments have been assigned to each of the params, 
      // perform the evaluation.  
      case EmptyLispList => body match {
        // If the body is a function, set the parent of the body's environment 
        // to the current environment and evalutae.
        case builtinFunc @ LispBuiltinFunction(_, _, _, NonEmptyEnvironment(vm, _)) =>
          builtinFunc.updated(environment = NonEmptyEnvironment(vm, environment)).evaluate
        case customFunc @ LispCustomFunction(_, _, _, _, NonEmptyEnvironment(vm, _)) =>
          customFunc.updated(environment = NonEmptyEnvironment(vm, environment)).evaluate
          
        // If the body is not a function, no need for environment updates.  
        case _ => body.evaluate
      }
        
      // If some parameters remained unbound...  
      case _ => Errors.notEnoughArguments(params.size)   
    }
    case ConsLispList(arg, argsTail) => params match {
      // Too many arguments provided, return an error  
      case EmptyLispList => Errors.extraArgs(params.size)
        
      // If parameter contains the '&' character...  
      case ConsLispList(param @ LispSymbol(Constants.VarArgsChar), _) =>
        if (params.size != 2)
          Errors.invalidFormat(s"Symbol ${Constants.VarArgsChar} is not followed by a single symbol.")
        else updated(
          params = EmptyLispList,
          args = EmptyLispList,
          environment = environment + (params.tail.head, args)
        ).evaluate
        
      case ConsLispList(param, paramsTail) =>
        updated(
          params = paramsTail,
          args = argsTail,
          environment = environment + (param, arg)
        ).evaluate
    }
  }
  
  def updated(symbol: LispSymbol = symbol,
               params: LispList = params,
               args: LispList = args,
               body: LispValue = body,
               environment: Environment = environment): LispValue = 
    LispCustomFunction(symbol, params, args, body, environment)
}