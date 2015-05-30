package meruem

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Constants {
  object LispTypeStrings {
    lazy final val LispValue = "LispValue"
    lazy final val Symbol = "Symbol"
    lazy final val Number = "Number"
    lazy final val String = "String"
    lazy final val Error = "Error"
    lazy final val List = "List"
    lazy final val Nil = "Nil"
    lazy final val Pair = "Pair"
    lazy final val Unquote = "Unquote"
    lazy final val Quasiquote = "Quasiquote"
    lazy final val Integer = "Integer"
    lazy final val Double = "Double"
    lazy final val Float = "Float"
    lazy final val Long = "Long"
    lazy final val Function = "Function"
    lazy final val Boolean = "Boolean"
    lazy final val Character = "Character"
    lazy final val DefMacro = "Macro"
  }
  
  object FunctionNames {
    lazy final val Add = "+"
    lazy final val Subtract = "-"
    lazy final val Multiply = "*"
    lazy final val Divide = "/"
    lazy final val Modulus = "%"
    lazy final val GetType = "type"
    lazy final val ToInt = "to-int"
    lazy final val ToLong = "to-long"
    lazy final val ToFloat = "to-float"
    lazy final val ToDouble = "to-double"
    lazy final val Equals = "="
    lazy final val Not = "not"
    lazy final val LessThan = "<"
    lazy final val GreaterThan = ">"
    lazy final val ToString = "to-string"
  }
  
  lazy final val VarArgsChar = "&"
  final val OpenParen = "("
  final val CloseParen = ")"
  
  lazy final val LispQuoteSymbol = LispSymbol("quote")
  lazy final val LispCondSymbol = LispSymbol("cond")
  lazy final val LispDefSymbol = LispSymbol("def")
  lazy final val LispDefunSymbol = LispSymbol("defun")
  lazy final val LispReadSymbol = LispSymbol("read")
  lazy final val LispLambdaSymbol = LispSymbol("lambda")
  lazy final val LispQuasiQuoteSymbol = LispSymbol("quasiquote")
  lazy final val LispUnquoteSymbol = LispSymbol("unquote")
  lazy final val LispDefMacroSymbol = LispSymbol("defmacro")
  lazy final val LispLoadSymbol = LispSymbol("load")
  
  object Keywords {
    lazy final val Def = "def"
    lazy final val Defun = "defun"
    lazy final val DefMacro = "defmacro"
    lazy final val Import = "import"
  }
}
