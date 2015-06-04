package meruem.builtins

import meruem._
import meruem.Utils._

/**
 * Created by ybamelcash on 5/26/2015.
 */
object Predicates {
  def isAtom(args: LispList, env: Environment) = checkArgsCount(args)(_ == 1) {
    LispBoolean(args match {
      case NilLispList => false
      case ConsLispList(_: LispList, _) => false
      case _ => true
    })
  }(env)

  def isSymbol(args: LispList, env: Environment) = isAtom(args, env) and LispBoolean(args.head match {
    case LispSymbol(_) | LispDefMacro(_) | _: LispFunction => true
    case _ => false
  })

  def isList(args: LispList, env: Environment) = checkArgsCount(args)(_ == 1)(LispBoolean(args match {
    case ConsLispList(_: LispList, _) => true
    case _ => false
  }))(env)
}
