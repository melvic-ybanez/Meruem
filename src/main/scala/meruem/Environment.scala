package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
import Environment._

trait Environment {
  
  def parent: Environment
  def valueMap: MapType
  def +(key: LispValue, lvalue: => LispValue): Environment
  def get(key: LispSymbol): LispValue
  def hasSymbol(key: LispSymbol): Boolean
  
  def whenNotdefined(sym: LispSymbol)(f: => LispValue): LispValue = 
    if (hasSymbol(sym)) Errors.alreadyDefined(sym) else f
}

object Environment {
  type MapType = Map[String, LispValue]
}

case object NilEnvironment extends Environment {
  def parent = throw new IllegalAccessException("Parent of empty envirionment")
  
  def valueMap = throw new IllegalAccessError("Value map of empty environment")
  
  def +(key: LispValue, lvalue: => LispValue): Environment = key match {
    case LispSymbol(sym) => new SomeEnvironment(Map(sym -> lvalue), NilEnvironment)
  } 
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)
  
  def hasSymbol(key: LispSymbol) = false
  
  def hasMacro(name: String) = false
} 

class SomeEnvironment(values: => MapType, val parent: Environment) extends Environment {
  lazy val valueMap = values
  
  def updated(newValueMap: => MapType = valueMap,
              newParent: Environment = parent) =
    new SomeEnvironment(newValueMap, newParent)
  
  def +(key: LispValue, lvalue: => LispValue) = key match {
    case LispSymbol(sym) => updated(newValueMap = valueMap + (sym -> lvalue))
  }
  
  def get(key: LispSymbol): LispValue = valueMap.getOrElse(key.value, parent.get(key))
  
  def hasSymbol(key: LispSymbol) = get(key) match {
    case error: LispError => false
    case _ => true
  }
}

case object SomeEnvironment {
  def apply(values: => MapType, parent: Environment) = 
    new SomeEnvironment(values, parent)
  
  def unapply(environment: SomeEnvironment) = Some(environment.valueMap, environment.parent)
}
