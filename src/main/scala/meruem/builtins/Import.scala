package meruem.builtins

import java.nio.file.{LinkOption, Paths, Files}

import meruem.Constants.{LispTypeStrings, Keywords}
import meruem.LispParser._
import meruem.Environment._
import meruem._
import meruem.Utils._
import meruem.builtins.Functions._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
 * Created by ybamelcash on 5/28/2015.
 */
case object Import extends (LispList => LispValue) {
  def apply(args: LispList) = withStringArg(args, Globals.environment) { filePath =>
    if (Files.exists(Paths.get(filePath))) {
      if (Globals.modules.contains(filePath)) LispNil
      else Utils.read(meruem, Source.fromFile(filePath).mkString) { llist =>
        type LispValues = List[LispValue]
        type Modules = List[Module]
        
        def evalNonDefExprs(exprs: LispList, 
                            modules: Modules, 
                            defs: LispValues, 
                            funcs: LispValues): Either[LispError, (Modules, LispValues, LispValues)] = exprs match {
          case NilLispList => Right(modules, defs, funcs)
            
          // If it's an import expression, apply the Import function to it.
          case (LispSymbol(Keywords.Import) !: args) !: tail => Import(args) match {
            case error: LispError => Left(error)
            case module: Module => evalNonDefExprs(tail, module :: modules, defs, funcs)
          }
            
          // If it's a define expression, save it for later.
          case define@((LispSymbol(sym) !: args) !: tail) if isDefineCommand(sym) =>
            if (sym == Keywords.Def) evalNonDefExprs(tail, modules, define :: defs, funcs)
            else evalNonDefExprs(tail, modules, defs, define :: funcs)
            
          // If the expression is neither import nor define, just evaluate it.  
          case lval !: tail => Evaluate(lval, Globals.environment) match {
            case error: LispError => Left(error)
            case lval => evalNonDefExprs(tail, modules, defs, funcs)
          }
        }
        
        evalNonDefExprs(llist, Nil, Nil, Nil) match {
          case Left(error) => error
          case Right(modules, defs, funcs) =>
            def ldef: LispDef = LispDef(environment)
            
            def evalFuncExprs(funcs: LispValues, valueMap: ValueMapType): Either[LispError, ValueMapType] = funcs match {
              case Nil => Right(valueMap)
              case (LispSymbol(op) !: (nameSym@LispSymbol(name)) !: params !: body) :: tail =>
                lambda(params !: body, Globals.environment) match {
                  case llambda: LispLambda => 
                    if (valueMap.exists(_._1 == name)) Left(Errors.alreadyDefined(nameSym))
                    else evalFuncExprs(tail, valueMap + (name -> {
                      val llambda1 = llambda.updated(environment = ldef.environment)
                      if (op == Keywords.Defun) llambda1 else LispDefMacro(llambda1)
                    }))
                  case error: LispError => Left(error)
                }
              case (_ !: name !: _) :: _ => Left(Errors.invalidType(LispTypeStrings.Symbol, name))
            }
            
            lazy val (errorOpt, environment) = evalFuncExprs(funcs, Globals.environment.valueMap) match {
              case Left(error) => (Some(error), NilEnvironment)
              case Right(valueMap) => (None, SomeEnvironment(valueMap, Globals.environment))
            }
            
            errorOpt.getOrElse(ldef)
        }
      }
    } else Errors.fileNotFound(filePath)
  }
}
