package meruem

import meruem.Utils._
import meruem.builtins.Functions._
import meruem.Constants._
import meruem.builtins.Import
import org.apache.commons.lang.StringEscapeUtils

/**
 * Created by ybamelcash on 5/4/2015.
 */
case object Evaluate extends ((LispValue, Environment) => LispValue) {
  def apply(lispValue: LispValue, environment: Environment): LispValue = lispValue match {
    // Symbol evaluates to whatever it is bound to in the environment  
    case symbol: LispSymbol => environment.get(symbol)

    // Strings get unescaped first before they get returned  
    case LispString(str) => LispString(StringEscapeUtils.unescapeJava(str))  
    
    // Self-evaluating expressions
    case error: LispError => error
    case atom: LispAtom[_] => atom
    case NilLispList => lispValue  
      
    // The first item of the list must be a symbol that corresponds to a function name,
    // a special operator, or a macro.
    case ConsLispList(head, tail) => Evaluate(head, environment) match {
      // Special functions/operators  
      case LispQuoteSymbol => quote(tail)
      case LispQuasiQuoteSymbol => quasiquote(tail, environment)
      case LispUnquoteSymbol => unquote(tail)
      case LispCondSymbol => cond(tail, environment)
      case LispReadSymbol => eval(tail, environment)
      case LispDefSymbol => define(tail, environment)
      case LispDefunSymbol => defun(tail, environment)
      case LispLambdaSymbol => lambda(tail, environment)
      case LispDefMacroSymbol => defmacro(tail, environment)
      
      // If the first symbol is a macro, expand it first before evaluating it.  
      case lmacro: LispDefMacro => Evaluate(macroExpand(lmacro !: tail, environment), environment)
        
      case LispBuiltinFunction(func) => sanitizeAll(tail, environment)(func) 
      case customFunc: LispLambda => Evaluate(customFunc.updated(args = tail), environment)
      case error: LispError => error
      case lval => Errors.nonFunction(lval)
    }
      
    case customFunc @ LispLambda(params, args, body, environ @ SomeEnvironment(vm, _)) =>
      def evaluateRest(sym: LispSymbol, lval: LispValue, 
                       params: LispList = NilLispList,
                       args: LispList = NilLispList) = 
        Evaluate(customFunc.updated(
          params = params,
          args = args,
          environment = environ + (sym, lval)
        ), environment)
      
      args match {
        case NilLispList => params match {
          // If each of the arguments have been assigned to each of the params, 
          // perform evaluation on the body.  
          case NilLispList => Evaluate(body, environ)
  
          // If parameter contains the '&' character...  
          case LispSymbol(Constants.VarArgsChar) !: (sym: LispSymbol) !: NilLispList =>
            evaluateRest(sym, NilLispList)   // bind the variable follwing the '&' character to empty list
          case LispSymbol(Constants.VarArgsChar) !: lval !: _ => Errors.invalidType(LispTypeStrings.Symbol, lval)
          case LispSymbol(Constants.VarArgsChar) !: _ => Errors.varArgsCount(params)
  
          // If some parameters remained unbound...  
          case _ => Errors.notEnoughArgs(params)
        }
        case arg !: argsTail => params match {
          // Too many arguments provided, return an error  
          case NilLispList => Errors.extraArgs(args)
  
          // If parameter contains the '&' character...  
          case LispSymbol(Constants.VarArgsChar) !: sym !: NilLispList =>
            val newArgs = args.map(Evaluate(_, environment))
            
            // make sure there are no errors in the arguments
            newArgs.find {
              case error: LispError => true
              case _ => false
            } getOrElse { 
              // if all the arguments are valid, bind them to the symbol following the '&' character
              sym match {
                case sym: LispSymbol => evaluateRest(sym, newArgs)
                case _ => Errors.invalidType(LispTypeStrings.Symbol, sym)
              }
            }
          case LispSymbol(Constants.VarArgsChar) !: _ => Errors.varArgsCount(params)
  
          case (param: LispSymbol) !: paramsTail => whenValid(Evaluate(arg, environment)) { arg =>
            evaluateRest(
              sym = param, 
              lval = arg,
              params = paramsTail,
              args = argsTail
            )
          }
        }
      }
  } 
}
