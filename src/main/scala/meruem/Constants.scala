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
    lazy final val Head = "head"
    lazy final val Tail = "tail"
    lazy final val Cons = "cons"
    lazy final val Cond = "cond"
    lazy final val Quote = "quote"
    lazy final val Quasiquote = "quasiquote"
    lazy final val Unquote = "unqoute"
    lazy final val List = "list"
    lazy final val AtomP = "atom?"
    lazy final val SymbolP = "symbol?"
    lazy final val ListP = "list?"
    lazy final val Macro = "macro"
    lazy final val LNil = "nil"
    lazy final val Eval = "eval"
    lazy final val Read = "read"
    lazy final val LError = "error"
    lazy final val LDefMacro = "defmacro"
    lazy final val ReadLine = "read-line"
    lazy final val Print = "print"
  }
  
  lazy final val VarArgsChar = "&"
  lazy final val OpenParen = "("
  lazy final val CloseParen = ")"
  lazy final val ModuleSeparator = "."
  lazy final val PathSeparator = "/"
  
  lazy final val LispQuoteSymbol = LispSymbol("quote")
  lazy final val LispCondSymbol = LispSymbol("cond")
  lazy final val LispDefSymbol = LispSymbol(Keywords.Def)
  lazy final val LispDefunSymbol = LispSymbol(Keywords.Defun)
  lazy final val LispEvalSymbol = LispSymbol("read")
  lazy final val LispLambdaSymbol = LispSymbol("lambda")
  lazy final val LispQuasiQuoteSymbol = LispSymbol("quasiquote")
  lazy final val LispUnquoteSymbol = LispSymbol("unquote")
  lazy final val LispDefMacroSymbol = LispSymbol(Keywords.DefMacro)
  lazy final val LispLoadSymbol = LispSymbol("load")
  lazy final val LispModuleSymbol = LispSymbol(Keywords.Module)
  
  object Keywords {
    lazy final val Def = "def"
    lazy final val Defun = "defun"
    lazy final val DefMacro = "defmacro"
    lazy final val Import = "import"
    lazy final val Lambda = "lambda"
    lazy final val Module = "module"
    lazy final val True = "true"
    lazy final val False = "false"
  }
}
