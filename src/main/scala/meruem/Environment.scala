package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */

import Environment._
import Constants._

trait Environment {
  def parent: Environment
  def valueMap: ValueMapType
  def + (key: LispSymbol, lvalue: LispValue): Environment
  def += (key: LispSymbol, lvalue: LispValue): Environment
  def get(key: LispSymbol): LispValue
  def hasSymbol(key: LispSymbol): Boolean
  
  def module: Module = this match {
    case NilEnvironment => throw new IllegalAccessException("module of nil environment")
    case env => env.get(LispModuleSymbol) match {
      case mod: Module => mod
      case error: LispError => NilModule
    }
  } 
  
  def whenNotdefined(sym: LispSymbol)(f: => LispValue): LispValue = 
    if (hasSymbol(sym)) Errors.alreadyDefined(sym)(this) else f
}

object Environment {
  type ValueMapType = collection.mutable.Map[String, LispValue]
}

case object NilEnvironment extends Environment {
  def parent = throwError("Parent")
  
  def valueMap = throwError("Value map")

  def += (key: LispSymbol, lval: LispValue) = throwError("+=")

  def + (key: LispSymbol, lval: LispValue) = 
    SomeEnvironment(collection.mutable.Map[String, LispValue](key.value -> lval), NilEnvironment)
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)(this)
  
  def hasSymbol(key: LispSymbol) = false
  
  def hasMacro(name: String) = false
  
  def throwError(member: String) = throw new IllegalAccessException(member + " of empty environment")
} 

case class SomeEnvironment(valueMap: ValueMapType, parent: Environment) extends Environment {
  def updated(newValueMap: ValueMapType = valueMap,
              newParent: Environment = parent) =
    SomeEnvironment(newValueMap, newParent)
  
  def + (key: LispSymbol, lvalue: LispValue) = add(key, lvalue)(_ + _)
  
  def += (key: LispSymbol, lvalue: LispValue) = add(key, lvalue)(_ += _)
  
  def add(key: LispSymbol, lvalue: LispValue)(f: (ValueMapType, (String, LispValue)) => ValueMapType) = 
    updated(newValueMap = f(valueMap, (key.value, lvalue)))
  
  def get(key: LispSymbol): LispValue = valueMap.getOrElse(key.value, parent.get(key)) match {
    case LispError(msg, _) =>
      // We need to return the outer error since it holds the info for the actual location of the erroneous code
      def error = LispError(msg, key)(this)
      
      if (key.value.contains(ModuleSeparator)) {
        val values = key.value.split(s"""\\$ModuleSeparator""")
        val modulePath = values.init.mkString(PathSeparator)
        module.submodules.find {
          case SomeModule(filePath, _, _) => filePath == modulePath
        } map {
          case SomeModule(filePath, _, environment) =>
            val function = values.last
            environment.get(LispSymbol(function)) match {
              case _: LispError => error
              case lval => lval
            }
        } getOrElse error
      } else error
    case lval => lval
  }
  
  def hasSymbol(key: LispSymbol) = get(key) match {
    case error: LispError => false
    case _ => true
  }
}
