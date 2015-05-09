package meruem

import meruem.Utils._
import meruem.builtins.Functions
import meruem.builtins.Functions._
import meruem.Constants._

/**
 * Created by ybamelcash on 5/4/2015.
 */
object Evaluate extends ((LispValue, Environment) => LispValue) {
  def apply(lispValue: LispValue, environment: Environment): LispValue = lispValue match {
    // Symbol evaluates to whatever it is bound to in the environment  
    case symbol: LispSymbol => environment.get(symbol)
    
    // Self-evaluating expressions
    case error: LispError => error
    case atom: LispAtom[_] => atom
    case EmptyLispList => lispValue
      
    // The first item of the list must be a symbol that corresponds to a function name  
    case ConsLispList(head, tail) => Evaluate(head, environment) match {
      case LispQuoteSymbol => quote(tail)
      case LispCondSymbol => cond(tail, environment)
      case LispReadSymbol => Functions.read(tail, environment)
      case LispDefSymbol => define(tail, environment)
      case LispLambdaSymbol => lambda(tail, environment)
      case LispBuiltinFunction(func) => sanitizeAll(tail, environment)(func) 
      case customFunc: LispCustomFunction => Evaluate(customFunc.updated(args = tail), environment)
      case error: LispError => error
      case lval => Errors.nonFunction(lval)
    }
      
    case customFunc @ LispCustomFunction(params, args, body, environ @ NonEmptyEnvironment(vm, _)) => args match {
      case EmptyLispList => params match {
        // If each of the arguments have been assigned to each of the params, 
        // perform evaluation on the body.  
        case EmptyLispList => Evaluate(body, environ)

        // If some parameters remained unbound...  
        case _ => Errors.notEnoughArguments(params.size)
      }
      case ConsLispList(arg, argsTail) => params match {
        // Too many arguments provided, return an error  
        case EmptyLispList => Errors.extraArgs(args.size)

        // If parameter contains the '&' character...  
        case ConsLispList(LispSymbol(Constants.VarArgsChar), ConsLispList(LispSymbol(xs), EmptyLispList)) =>
          val newArgs = args.map(Evaluate(_, environment))
          
          // make sure there are no errors in the arguments
          newArgs.find {
            case error: LispError => true
            case _ => false
          } getOrElse {
            // if all the arguments are valid, bind them to the symbol following the '&' character
            Evaluate(customFunc.updated(
              params = EmptyLispList,
              args = EmptyLispList,
              environment = environ.updated(newValueMap = vm + (xs -> newArgs))
            ), environment)
          }
        case ConsLispList(param @ LispSymbol(Constants.VarArgsChar), _) =>
          Errors.invalidFormat(s"Symbol ${Constants.VarArgsChar} is not followed by a single symbol.")

        case ConsLispList(param, paramsTail) => whenValid(Evaluate(arg, environment)) { arg =>
          Evaluate(customFunc.updated(
            params = paramsTail,
            args = argsTail,
            environment = environ + (param, arg)
          ), environment)
        }
      }
    }
  } 
}
