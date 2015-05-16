package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
trait Environment {
  def parent: Environment
  def valueMap: Map[String, LispValue]
  def +(key: LispValue, lvalue: => LispValue): Environment
  def get(key: LispSymbol): LispValue
  def hasSymbol(key: LispSymbol): Boolean
  
  def whenNotdefined(sym: LispSymbol)(f: => LispValue): LispValue = 
    if (hasSymbol(sym)) Errors.alreadyDefined(sym) else f
}

case object EmptyEnvironment extends Environment {
  def parent = throw new IllegalAccessException("Parent of empty envirionment")
  
  def valueMap = throw new IllegalAccessError("Value map of empty environment")
  
  def +(key: LispValue, lvalue: => LispValue): Environment = key match {
    case LispSymbol(sym) => new NonEmptyEnvironment(Map(sym -> lvalue), EmptyEnvironment)
  } 
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)
  
  def hasSymbol(key: LispSymbol) = false
  
  def hasMacro(name: String) = false
} 

class NonEmptyEnvironment(values: => Map[String, LispValue], val parent: Environment) extends Environment {
  lazy val valueMap = values
  
  def updated(newValueMap: => Map[String, LispValue] = valueMap,
              newParent: Environment = parent) =
    new NonEmptyEnvironment(newValueMap, newParent)
  
  def +(key: LispValue, lvalue: => LispValue) = key match {
    case LispSymbol(sym) => updated(newValueMap = valueMap + (sym -> lvalue))
  }
  
  def get(key: LispSymbol): LispValue = valueMap.getOrElse(key.value, parent.get(key))
  
  def hasSymbol(key: LispSymbol) = get(key) match {
    case error: LispError => false
    case _ => true
  }
}

case object NonEmptyEnvironment {
  def apply(values: => Map[String, LispValue], parent: Environment) = 
    new NonEmptyEnvironment(values, parent)
  
  def unapply(environment: NonEmptyEnvironment) = Some(environment.valueMap, environment.parent)
}
