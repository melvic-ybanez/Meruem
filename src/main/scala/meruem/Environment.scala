package meruem

/**
 * Created by ybamelcash on 4/26/2015.
 */

import java.io.File
import java.nio.file.Paths

import meruem.Constants._
import meruem.Environment._

trait Environment {
  def parent: Environment
  def valueMap: ValueMapType
  def + (key: LispSymbol, lvalue: LispValue): Environment
  def += (key: LispSymbol, lvalue: LispValue): Environment
  def hasSymbol(key: LispSymbol): Boolean
  
  def module(): Module = get(ModuleSymbol) match {
    case mod: Module => mod
    case error: LispError => NilModule
  }

  def get(key: LispSymbol): LispValue = {
    def recurse(env: Environment): LispValue = env match {
      case NilEnvironment => Errors.unboundSymbol(key)(this)
      case SomeEnvironment(valueMap, parent) => valueMap.getOrElse(key.value, recurse(env.parent))
    }

    recurse(this) match {
      case LispError(msg, _) => 
        // We need to return the outer error since it holds the info for the actual 
        // location of the erroneous code
        def error = LispError(msg, key)(this)

        if (key.value.contains(ModuleSeparator)) {
          val values = key.value.split(s"""\\$ModuleSeparator""")
          val modulePath = values.init.mkString(File.separator)
          val mod = module()
          mod.submodules.find {
            case SomeModule(filePath, _, _) =>
              val relativeParent = Option(Paths.get(mod.filePath).getParent).map(_ + File.separator).getOrElse("")
              filePath == modulePath ||
                filePath == Paths.get(relativeParent).resolve(modulePath).toString || 
                filePath == Paths.get(Settings.libLocation).resolve(modulePath).toString
          } map {
            case SomeModule(filePath, _, environment) =>
              val function = values.last
              environment.get(LispSymbol(function)) match {
                case _: LispError => error
                case lval => lval
              }
          } getOrElse error 
        } else get(LispSymbol(Globals.preloadedString + ModuleSeparator + key.value)) match {
          case _: LispError => error
          case lval => lval
        }
      case lval => lval
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
  
  def hasSymbol(key: LispSymbol) = false
  
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
  
  def hasSymbol(key: LispSymbol) = get(key) match {
    case error: LispError => false
    case _ => true
  }
}
