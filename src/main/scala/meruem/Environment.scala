package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
trait Environment {
  def parent: Environment
  def valueMap: Map[String, LispValue]
  def +(key: LispValue, lvalue: LispValue): Environment
  def get(key: LispSymbol): LispValue
  def hasSymbol(key: LispSymbol): Boolean
}

case object EmptyEnvironment extends Environment {
  def parent = throw new IllegalAccessException("Empty environment has no parent")
  
  def valueMap = Map()
  
  def +(key: LispValue, lvalue: LispValue): Environment = key match {
    case LispSymbol(sym) => NonEmptyEnvironment(Map(sym -> lvalue), EmptyEnvironment)
  } 
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)
  
  def hasSymbol(key: LispSymbol) = false
} 

case class NonEmptyEnvironment(valueMap: Map[String, LispValue], parent: Environment) extends Environment {
  def updated(newValueMap: Map[String, LispValue] = valueMap,
              newParent: Environment = parent) =
    NonEmptyEnvironment(newValueMap, newParent)
  
  def +(key: LispValue, lvalue: LispValue) = key match {
    case LispSymbol(sym) => updated(newValueMap = valueMap + (sym -> lvalue))
  }
  
  def get(key: LispSymbol): LispValue = valueMap.getOrElse(key.value, parent.get(key))
  
  def hasSymbol(key: LispSymbol) = get(key) match {
    case error: LispError => false
    case _ => true
  }
}
