package meruem

import meruem.Utils.Aliases._

/**
 * Created by ybamelcash on 4/26/2015.
 */
sealed trait LispValue {
  def evaluate: LispValue
}

sealed trait LispAtom[A] extends LispValue {
  def value: A
  
  def evaluate = this
  
  override def toString = value.toString
}

case class LispString(value: String) extends LispAtom[String]

case class LispNumber(value: Long) extends LispAtom[Long]

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
  def ::[B >: A <: LispValue](lval: B): LispList[B]
  
  def map[B <: LispValue](f: A => B): LispList[B]
  def find(p: A => Boolean): Option[A]
  def size: Int

  def exists(p: A => Boolean) = find(p).nonEmpty
  
  def filter(p: A => Boolean): LispList[A] = foldLeft(LispList[A]()) { (acc, h) =>
    if (p(h)) h :: acc else acc
  }
  
  def foldLeft[B <: LispValue](initialValue: B)(f: (B, A) => B) = {
    def recurse(llist: LispList[A], acc: B): B = llist match {
      case EmptyLispList => acc
      case ConsLispList(h, t) => recurse(t, f(acc, h))
    }
    
    recurse(this, initialValue)
  }
  
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
  
  def ::[B >: Nothing <: LispValue](lval: B) = ConsLispList(lval, EmptyLispList)
  
  def map[B <: LispValue](f: Nothing => B) = EmptyLispList
  
  def find(p: Nothing => Boolean) = None
  
  def size = 0
  
  def evaluate = this
}

case class ConsLispList[A <: LispValue](head: A, tail: LispList[A]) extends LispList[A] {
  def ::[B >: A <: LispValue](lval: B) = ConsLispList(lval, this)
  
  def map[B <: LispValue](f: A => B) = f(head) :: tail.map(f)
  
  def find(p: A => Boolean) = 
    if (p(head)) Some(head)
    else tail.find(p)
  
  def size = 1 + tail.size
  
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
    def checkVarArgs(args: LispValueList): LispValue = 
      if (params.size != 2)
        Errors.invalidFormat(s"Symbol ${Constants.VarArgsChar} is not followed by a single symbol.")
      else updated(
        params = EmptyLispList,
        args = EmptyLispList,
        environment = environment + (params.tail.head, args.map(_.evaluate))
      ).evaluate
    
    args match {
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
            environment = environment + (param, arg.evaluate)
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