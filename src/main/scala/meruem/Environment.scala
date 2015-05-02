package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */
trait Environment {
  def parent: Environment
  def valueMap: Map[LispSymbol, LispValue]
  def +(key: LispSymbol, lvalue: LispValue): Environment
  def get(key: LispSymbol): LispValue
}

case object EmptyEnvironment extends Environment {
  def parent = throw new IllegalAccessException("Empty environment has no parent")
  
  def valueMap = Map()
  
  def +(key: LispSymbol, lvalue: LispValue) = 
    NonEmptyEnvironment(Map(key -> lvalue), EmptyEnvironment)
  
  def get(key: LispSymbol) = Errors.unboundSymbol(key)
} 

case class NonEmptyEnvironment(valueMap: Map[LispSymbol, LispValue], parent: Environment) extends Environment {
  def updated(newValueMap: Map[LispSymbol, LispValue] = valueMap,
              newParent: Environment = parent) =
    NonEmptyEnvironment(newValueMap, newParent)
  
  def +(key: LispSymbol, lvalue: LispValue) = updated(newValueMap = valueMap + (key -> lvalue))
  
  def get(key: LispSymbol) = valueMap.get(key).getOrElse(parent.get(key))
}
