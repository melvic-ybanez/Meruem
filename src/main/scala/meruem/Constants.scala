package meruem

/**
 * Created by ybamelcash on 4/27/2015.
 */
object Constants {
  import meruem.Constants.BuiltinModuleNames._
  
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
    lazy final val IsAtom = "atom?"
    lazy final val IsSymbol = "symbol?"
    lazy final val IsList = "list?"
    lazy final val Macro = "macro"
    lazy final val Eval = "eval"
    lazy final val Read = "read"
    lazy final val LError = "error"
    lazy final val LDefMacro = "defmacro"
    lazy final val ReadLine = "read-line"
    lazy final val Print = "print"
    lazy final val ToPath = s"$Paths.to-path"
    lazy final val PathFileName = s"$Paths.filename"
    lazy final val PathsName = s"$Paths..name"
    lazy final val PathsNameCount = s"$Paths.name-count"
    lazy final val PathsSubpath = s"$Paths.subpath"
    lazy final val PathsGetParent = s"$Paths.parent"
    lazy final val PathsGetRoot = s"$Paths.root"
    lazy final val PathsNormalize = s"$Paths.normalize"
    lazy final val PathsToURI = s"$Paths.to-uri"
    lazy final val PathsToAbsolute = s"$Paths.to-absolute"
    lazy final val PathsToReal = s"$Paths.to-real"
    lazy final val PathsResolve = s"$Paths.resolve"
    lazy final val PathsRelativize = s"$Paths.relativize"
    lazy final val FilesExists = s"$Files.exists?"
    lazy final val FilesIsReadable = s"$Files.readable?"
    lazy final val FilesIsWritable = s"$Files.writable?"
    lazy final val FilesIsExecutable = s"$Files.executable?"
    lazy final val FilesDelete = s"$Files.delete"
    lazy final val FilesCopy = s"$Files.copy"
    lazy final val FilesSize = s"$Files.size"
    lazy final val FilesIsDirectory = s"$Files.directory?"
    lazy final val FilesIsHidden = s"$Files.hidden?"
    lazy final val FilesList = s"$Files.list"
    lazy final val Apply = "apply"
    lazy final val FilesReadLines = s"$Files.read-lines"
    lazy final val FilesWrite = s"$Files.write"
    lazy final val TailRec = "tail-rec"
    lazy final val Gensym = "gensym"
    lazy final val MathPI = s"$Math.Pi"
    lazy final val MathCos = s"$Math.cos"
    lazy final val MathSin = s"$Math.sin"
    lazy final val MathTan = s"$Math.tan"
    lazy final val MathCeil = s"$Math.ceil"
    lazy final val MathExp = s"$Math.exp"
    lazy final val MathFloor = s"$Math.floor"
    lazy final val MathLog = s"$Math.log"
    lazy final val MathLog10 = s"$Math.log10"
    lazy final val MathMax = s"$Math.max"
    lazy final val MathMin = s"$Math.min"
    lazy final val MathPow = s"$Math.pow"
    lazy final val MathRandom = s"$Math.random"
    lazy final val MathSqrt = s"$Math.sqrt"
  }
  
  lazy final val VarArgsChar = "&"
  lazy final val OpenParen = "("
  lazy final val CloseParen = ")"
  lazy final val ModuleSeparator = "."
  lazy final val PairListOpenParen = "{"
  lazy final val PairListCloseParen = "}"
  
  lazy final val QuoteSymbol = LispSymbol(FunctionNames.Quote)
  lazy final val CondSymbol = LispSymbol(FunctionNames.Cond)
  lazy final val DefSymbol = LispSymbol(Keywords.Def)
  lazy final val LambdaSymbol = LispSymbol(Keywords.Lambda)
  lazy final val QuasiQuoteSymbol = LispSymbol(FunctionNames.Quasiquote)
  lazy final val UnquoteSymbol = LispSymbol(FunctionNames.Unquote)
  lazy final val DefMacroSymbol = LispSymbol(Keywords.DefMacro)
  lazy final val ImportSymbol = LispSymbol(Keywords.Import)
  lazy final val ModuleSymbol = LispSymbol(Keywords.Module)
  lazy final val TryCatchSymbol = LispSymbol(Keywords.TryCatch)
  lazy final val ApplySymbol = LispSymbol(FunctionNames.Apply)
  lazy final val LetSymbol = LispSymbol(Keywords.Let)
  lazy final val TailRecSymbol = LispSymbol(FunctionNames.TailRec)
  lazy final val MainSymbol = LispSymbol(Settings.mainFunction)
  
  object Keywords {
    lazy final val Def = "def"
    lazy final val DefMacro = "defmacro"
    lazy final val Import = "import"
    lazy final val Lambda = "lambda"
    lazy final val Module = "module"
    lazy final val True = "true"
    lazy final val False = "false"
    lazy final val TryCatch = "try-catch"
    lazy final val Error = "error"
    lazy final val Let = "let"
    lazy final val Recur = "recur"
    lazy final val LNil = "nil"
  }
  
  object BuiltinModuleNames {
    lazy final val Paths = "paths"
    lazy final val Files = "files"
    lazy final val Math = "math"
  }
}
