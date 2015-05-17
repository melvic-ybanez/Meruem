package meruem

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Constants {
  object LispTypeStrings {
    final val LispValue = "LispValue"
    final val Symbol = "Symbol"
    final val Number = "Number"
    final val String = "String"
    final val Error = "Error"
    final val List = "List"
    final val Nil = "Nil"
    final val Pair = "Pair"
    final val Unquote = "Unquote"
    final val Quasiquote = "Quasiquote"
  }
  
  final val VarArgsChar = "&"
  final val ListOpenParen = "("
  final val ListCloseParen = ")"
  
  final val LispQuoteSymbol = LispSymbol("quote")
  final val LispCondSymbol = LispSymbol("cond")
  final val LispDefSymbol = LispSymbol("def")
  final val LispDefunSymbol = LispSymbol("defun")
  final val LispReadSymbol = LispSymbol("read")
  final val LispLambdaSymbol = LispSymbol("lambda")
  final val LispQuasiQuoteSymbol = LispSymbol("quasiquote")
  final val LispUnquoteSymbol = LispSymbol("unquote")
  final val LispMacroSymbol = LispSymbol("macro")
  final val LispDefMacroSymbol = LispSymbol("defmacro")
}
