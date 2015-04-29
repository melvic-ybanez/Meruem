package meruem

import meruem.Utils.Aliases._

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue {
  def evaluate: LispValue
  
  def postEvaluation(f: LispValue => LispValue): LispValue = this match {
    case error: LispError => error
    case  lval: LispValue => f(lval)
  }
}

sealed trait LispAtom[A] extends LispValue {
  def value: A
  
  def evaluate = this
  
  override def toString = value.toString
}

case class LispString(value: String) extends LispAtom[String]

case class LispNumber(value: Long) extends LispAtom[Long]

case class LispBoolean(value: Boolean) extends LispAtom[Boolean]

case class LispError(value: String) extends LispValue {
  def evaluate = this
  
  override def toString = "Error: " + value
}

case class LispSymbol(value: String) extends LispAtom[String] {
  override def evaluate = ??? 
}

sealed trait LispList[+A <: LispValue] extends LispValue {
  def head: A
  def tail: LispList[A]
  
  def ::[B >: A <: LispValue](lval: B): LispList[B] = this match {
    case EmptyLispList => LispList(lval)
    case _: ConsLispList => ConsLispList(lval, this) 
  }
  
  def ++[B >: A <: LispValue](llist: LispList[B]): LispList[B] = 
    reverse.foldLeft(llist)((acc, h) => h :: acc)
  
  def find(p: A => Boolean): Option[A] = {
    def recurse(llist: LispList[A]): Option[A] = llist match {
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

  def exists(p: A => Boolean) = find(p).nonEmpty
  
  def forAll(p: A => Boolean) = !exists(!p(_))
  
  def filter(p: A => Boolean): LispList[A] = foldLeft(LispList[A]()) { (acc, h) =>
    if (p(h)) h :: acc else acc
  }

  def map[B <: LispValue](f: A => B): LispList[B] = {
    def recurse(llist: LispList[A], acc: LispList[B]): LispList[B] = llist match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, f(h) :: acc)
    }
    
    recurse(this, EmptyLispList).reverse
  }
  
  def foldLeft[B <: LispValue](initialValue: B)(f: (B, A) => B) = {
    def recurse(llist: LispList[A], acc: B): B = llist match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, f(acc, h))
    }
    
    recurse(this, initialValue)
  }
  
  def reverse: LispList[A] = foldLeft(LispList[A]())((acc, h) => h :: acc)
  
  override def toString = {
    def recurse(xs: LispList[A], acc: List[String]): List[String] = xs match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, h.toString :: acc)
    } 
    
    val str = recurse(this, Nil).reverse.mkString
    Constants.ListOpenParen + str + Constants.ListCloseParen  
  }
}

case object EmptyLispList extends LispList[Nothing] {
  def head = throw new IllegalAccessException("Empty lisp list has no head.")
  
  def tail = throw new IllegalAccessException("Empty lisp list has no tail.")
  
  def evaluate = this
}

case class ConsLispList[A <: LispValue](head: A, tail: LispList[A]) extends LispList[A] {
  def evaluate = head.evaluate match {
    case builtinFunc: LispBuiltinFunction => builtinFunc.updated(args = tail).evaluate
    case customFunc: LispCustomFunction => customFunc.updated(args = tail).evaluate
    case lval => Errors.nonFunction(lval) 
  }
}

object LispList {
  def apply[A <: LispValue](lval: A*): LispList[A] = 
    if (lval.isEmpty) EmptyLispList
    else lval.foldLeft(LispList[A]())((llist, h) => h :: llist)
}

sealed trait LispFunction extends LispValue {
  def symbol: LispSymbol
  def args: LispValueList
  def environment: Environment
  
  override def toString = s"<function ${symbol.value}>"
}

case class LispBuiltinFunction(symbol: LispSymbol, 
                               args: LispValueList, 
                               func: LispValueList => LispValue,
                               environment: Environment) extends LispFunction {
  def evaluate = func(args)
  
  def updated(symbol: LispSymbol = symbol,
               args: LispValueList = args,
               func: LispValueList => LispValue = func,
               environment: Environment = environment) =
    LispBuiltinFunction(symbol, args, func, environment)
}

case class LispCustomFunction(symbol: LispSymbol, 
                              params: LispSymbolList, 
                              args: LispValueList,
                              body: LispValue,
                              environment: Environment) extends LispFunction {
  def evaluate = {
    def evaluateArgs: LispValueList = {
      def recurse(args: LispValueList, acc: LispValueList): LispValueList = args match {
        case EmptyLispList => acc
        case error: LispError => error
        case ConsLispList(h, t) => recurse(t, h :: acc)
      }
      
      recurse(args, EmptyLispList).reverse
    }
    
    def checkVarArgs(args: LispValueList): LispValue = 
      if (params.size != 2)
        Errors.invalidFormat(s"Symbol ${Constants.VarArgsChar} is not followed by a single symbol.")
      else updated(
        params = EmptyLispList,
        args = EmptyLispList,
        environment = environment + (params.tail.head, args)
      ).evaluate
    
    sanitizeAll(args) match {
      case EmptyLispList => params match {   
        case EmptyLispList => body match {    
          case builtinFunc @ LispBuiltinFunction(_, _, _, NonEmptyEnvironment(vm, _)) =>
            builtinFunc.updated(environment = NonEmptyEnvironment(vm, environment)).evaluate
          case customFunc @ LispCustomFunction(_, _, _, _, NonEmptyEnvironment(vm, _)) =>
            customFunc.updated(environment = NonEmptyEnvironment(vm, environment)).evaluate
          case _ => body.evaluate
        }
        case ConsLispList(param @ LispSymbol(Constants.VarArgsChar), _) => checkVarArgs(EmptyLispList)
        case p: ConsLispList => p   
      }
      case ConsLispList(arg, argsTail) => params match {
        case EmptyLispList => Errors.extraArgs(params.size)
        case ConsLispList(param @ LispSymbol(Constants.VarArgsChar), _) => checkVarArgs(args)
        case ConsLispList(param, paramsTail) =>
          updated(
            params = paramsTail,
            args = argsTail,
            environment = environment + (param, arg)
          ).evaluate
      }
    }
  }
  
  def updated(symbol: LispSymbol = symbol,
               params: LispSymbolList = params,
               args: LispValueList = args,
               body: LispValue = body,
               environment: Environment = environment): LispValue = 
    LispCustomFunction(symbol, params, args, body, environment)
}