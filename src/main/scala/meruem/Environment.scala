package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
trait Environment {
  def parent: Environment
  def valueMap: Map[LispSymbol, LispValue]
  def +(key: LispValue, lvalue: LispValue): Environment
  def get(key: LispSymbol): LispValue
}

case object EmptyEnvironment extends Environment {
  def parent = throw new IllegalAccessException("Empty environment has no parent")
  
  def valueMap = Map()
  
  def +(key: LispValue, lvalue: LispValue): Environment = key match {
    case sym: LispSymbol => NonEmptyEnvironment(Map(sym -> lvalue), EmptyEnvironment)
  } 
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)
} 

case class NonEmptyEnvironment(valueMap: Map[LispSymbol, LispValue], parent: Environment) extends Environment {
  def updated(newValueMap: Map[LispSymbol, LispValue] = valueMap,
              newParent: Environment = parent) =
    NonEmptyEnvironment(newValueMap, newParent)
  
  def +(key: LispValue, lvalue: LispValue) = key match {
    case sym: LispSymbol => updated(newValueMap = valueMap + (sym -> lvalue))
  }
  
  def get(key: LispSymbol): LispValue = valueMap.getOrElse(key, parent.get(key))
}
