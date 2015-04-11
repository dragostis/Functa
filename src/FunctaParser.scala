import org.parboiled2._

trait Value

case class Assignment(names: Seq[String], values: Seq[Value]) extends Value

case class ValueList(values: List[Value]) extends Value
case class Call(name: String, values: Seq[Value] = Seq()) extends Value

case class Access(value: Value, calls: Seq[Call]) extends Value

case class Function(arguments: Seq[String], defaults: Option[Seq[Assignment]], value: Value) extends Value

case class Block(statements: Seq[Value]) extends Value
case class Dictionary(assignments: Seq[Assignment]) extends Value

case class BooleanLiteral(value: String) extends Value
case class IntegerLiteral(value: String) extends Value
case class FloatLiteral(value: String) extends Value
case class StringLiteral(value: String) extends Value
case class SymbolLiteral(value: String) extends Value

class FunctaParser(val input: ParserInput) extends Parser {
  import CharPredicate.{Alpha, Digit, Digit19}

  def comment          = rule('#' ~ ((!newLine ~ ANY.named("comment body")) *))
  def whiteSpace       = CharPredicate(" \t\f")
  def newLine          = rule(('\r' ?) ~ '\n')
  def semicolon        = rule(';')

  def space          = rule(whiteSpace *)
  def breakableSpace = rule((((comment ?) ~ newLine.named("new line after comment")) | whiteSpace) *)

  def inlineSeparator   = rule(';' ~ space)
  def nextLineSeparator = rule(((comment ?) ~ newLine) ~ breakableSpace)

  def statementSeparator = rule(inlineSeparator | nextLineSeparator)
  def elementSeparator   = rule(',' ~ quiet(breakableSpace))

  def program = rule(statements ~ EOI.named("end of input"))

  def statement  = rule(value ~ quiet(space))
  def statements = rule(quiet(breakableSpace) ~ ((statement ~ quiet(space)) +).separatedBy(quiet(statementSeparator)) ~
    quiet(breakableSpace))

  def assignment  = rule((identifiers ~ quiet(space) ~ ':' ~ quiet(breakableSpace) ~ values) ~> Assignment)
  def assignments = rule(quiet(breakableSpace) ~ ((assignment ~ quiet(space)) *).separatedBy(quiet(elementSeparator)) ~
    quiet(breakableSpace))

  def block      = rule('{' ~ statements.named("statement") ~ '}' ~> Block)
  def dictionary = rule('[' ~ assignments.named("assignment") ~ ']' ~> Dictionary)

  def emptyCall     = rule(capture(identifier) ~> (identifier => Call(identifier)))
  def parenCall     = rule(capture(identifier) ~ '(' ~ quiet(breakableSpace) ~ values.named("arguments") ~
    quiet(breakableSpace) ~ ')' ~> Call)
  def parenlessCall = rule(capture(identifier) ~ quiet(whiteSpace) ~ quiet(space) ~ values.named("arguments") ~>
    Call)
  def call          = rule((parenCall.named("call") | parenlessCall.named("call") | emptyCall.named("call")))

  def access = rule(nonAccess ~ '.' ~ (call +).separatedBy('.') ~> Access)

  def arguments = rule(((capture(identifier) ~ quiet(space) ~ !':') +).separatedBy(quiet(elementSeparator)))
  def inputs    = rule(arguments ~ quiet(space) ~
    ((',' ~ quiet(breakableSpace) ~ assignments ~ quiet(space)) ?))
  def function  = rule(inputs ~ "=>" ~ quiet(breakableSpace) ~ expression ~> Function)

  def parens = rule('(' ~ value ~ ')')

  def constant      = rule(floatLiteral | integerLiteral| booleanLiteral | stringLiteral | symbolLiteral)
  def nonAccess     = rule(call | block | dictionary | constant)
  def nonExpression = rule(access | nonAccess)
  def expression    = rule(nonExpression.named("non-expression"))

  def value: Rule1[Value] = rule(parens | function | assignment | expression)
  def values              = rule(((value ~ quiet(space)) +).separatedBy(quiet(elementSeparator)))

  def integerLiteral = rule(capture(integer) ~> IntegerLiteral)
  def floatLiteral   = rule(capture(float)   ~> FloatLiteral)
  def booleanLiteral = rule(capture(boolean) ~> BooleanLiteral)
  def stringLiteral  = rule(capture(string)  ~> StringLiteral)
  def symbolLiteral  = rule(capture(symbol)  ~> SymbolLiteral)

  def identifier  = rule((letter | '_') ~ ((letter | digit | '_') *))
  def identifiers = rule(((capture(identifier) ~ quiet(space)) +).separatedBy(quiet(elementSeparator)))

  def boolean = rule(quiet("true") | quiet("false"))
  def integer = rule(('-' ?) ~ (quiet(digit19) ~ oneOrMore(digit) | digit))
  def float   = rule(('-' ?) ~ ((digit +) ~ '.' ~ (digit +) | '.' ~ (digit +)) ~ (exponent ?))
  def string  = rule('"' ~ (('\\' ~ escapeCharacter | !'"' ~ ANY) *) ~ '"')
  def symbol  = rule(':' ~ identifier)

  def exponent        = rule('e' ~ anyOf("+-") ~ (digit +))
  def escapeCharacter = rule(anyOf("\"\\nrtf"))
  
  def digit   = rule(Digit)
  def digit19 = rule(Digit19)
  def letter  = rule(Alpha)
}


//object FunctaParser extends RegexParsers {
//  override val skipWhitespace = false
//
//  def emptyLine: Parser[Statement] = "".r ^^ (_ => EmptyLine)
//  def comment: Parser[String] = "#".r ~> ".*".r
//
//  def separator: Parser[String] = ",".r
//  def statementSeparator: Parser[String] = """\n|(\n\s*\n)|;""".r
//  def dictionarySeparator: Parser[String] = """\n|(\n\s*\n)|,""".r
//  def accessor: Parser[String] = "\\.".r
//
//  def program: Parser[List[Statement]] = repsep(statement, statementSeparator)
//
//  def statement: Parser[Statement] =  (value | emptyLine) <~ (comment ?)
//
//  def access: Parser[Access] = (call | identifier) ~ accessor ~ rep1sep(call | identifier, accessor) ^^ {
//    case head ~ _ ~ tail => Access(head :: tail)
//  }
//
//  def assignment: Parser[Assignment] = identifier ~ ":".r ~ value ^^ {
//    case identifier ~ _ ~ value => Assignment(identifier, value)
//  }
//  def assignmentList: Parser[AssignmentList] = repsep(identifier, separator) ~ ":".r ~ repsep(value, separator) ^^ {
//    case identifiers ~ _ ~ values => AssignmentList(identifiers.zip(values).map {
//      case (identifier, value) => Assignment(identifier, value)
//    })
//  }
//
//  def valueList: Parser[ValueList] = "\\(".r ~> repsep(value, separator) <~ "\\)".r ^^ (list => ValueList(list))
//  def values: Parser[List[Value]] = rep1sep(valueList | block | dictionary, separator)
//
//  def call: Parser[Call] = identifier ~ values ^^ {
//    case identifier ~ values => Call(identifier.name, values)
//  }
//
//  def expandedArguments: Parser[Identifier] = "\\*".r ~> identifier
//  def expandedDefaults: Parser[Identifier] = "&".r ~> identifier
//
//  def explicitArguments: Parser[List[Identifier]] = rep1sep(identifier, separator)
//  def explicitDefaults: Parser[List[Assignment]] = rep1sep(assignment, separator)
//
//  def explicitOnly[T](explicitParser: => Parser[List[T]]): Parser[(List[T], Option[Identifier])] = {
//    explicitParser ^^ (list => (list, None))
//  }
//  def expandedOnly[T](expandedParser: => Parser[Identifier]): Parser[(List[T], Option[Identifier])] = {
//    expandedParser ^^ (identifier => (List(), Some(identifier)))
//  }
//  def explicitAndExpanded[T](
//    explicitParser: => Parser[List[T]],
//    expandedParser: => Parser[Identifier]
//  ): Parser[(List[T], Option[Identifier])] = {
//    explicitParser ~ separator ~ expandedParser ^^ {
//      case list ~ _ ~ identifier => (list, Some(identifier))
//    }
//  }
//
//  def arguments: Parser[(List[Identifier], Option[Identifier])] = {
//    explicitAndExpanded(explicitArguments, expandedArguments) |
//    explicitOnly(explicitArguments) |
//    expandedOnly[Identifier](expandedArguments)
//  }
//  def defaults: Parser[(List[Assignment], Option[Identifier])] = {
//    explicitAndExpanded(explicitDefaults, expandedDefaults) |
//    explicitOnly(explicitDefaults) |
//    expandedOnly[Assignment](expandedDefaults)
//  }
//
//  def argumentsOnly: Parser[InputList] = arguments ^^ {
//    case (arguments, expandedArguments) => InputList(arguments = arguments, expandedArguments = expandedArguments)
//  }
//  def defaultsOnly: Parser[InputList] = defaults ^^ {
//    case (defaults, expandedDefaults) => InputList(defaults = defaults, expandedDefaults = expandedDefaults)
//  }
//  def argumentsList: Parser[InputList] = "\\(".r ~> (argumentsOnly ?) <~ "\\)".r ^^ {
//    case Some(inputList) => inputList
//    case None => InputList()
//  }
//  def defaultsList: Parser[InputList] = "\\[".r ~> (defaultsOnly ?) <~ "\\]".r ^^ {
//    case Some(inputList) => inputList
//    case None => InputList()
//  }
//
//  def inputs: Parser[List[InputList]] = rep1sep(argumentsList | defaultsList, separator)
//
//  def function: Parser[Function] = inputs ~ "=>".r ~ value ^^ {
//    case arguments ~ _ ~ value => Function(arguments, value)
//  }
//
//  def block: Parser[Block] = "\\{\n*".r ~> rep1sep(statement, statementSeparator) <~  "\n*\\}".r ^^ {
//    statements => Block(statements)
//  }
//  def dictionary: Parser[Dictionary] = "\\[\n*".r ~> rep1sep(assignment, dictionarySeparator) <~ "\n*\\]".r ^^ {
//    assignment => Dictionary(assignment)
//  }
//
//  def binaryExpression(operator: Regex, nextExpression: Parser[Value], continue: Boolean = true): Parser[Value] = {
//    def expression: Parser[Access] = nextExpression ~ ((operator ~ nextExpression) +) ^^ {
//      case head ~ list => Access(list.foldLeft(List[Value](head)) {
//        case (list, operator ~ element) => Call(operator, List(element)) :: list
//      }.reverse)
//    }
//
//    if (continue) expression | nextExpression else expression
//  }
//
//  def expression: Parser[Value] =  or
//
//  def booleanExpression: Parser[Value] = or
//  def numericExpression: Parser[Value] = addition
//
//  def range: Parser[Value] = binaryExpression("\\-?\\-\\>".r, addition)
//  def or: Parser[Value] = binaryExpression("\\|\\|".r, and)
//  def and: Parser[Value] = binaryExpression("&&".r, equal)
//  def equal: Parser[Value] = binaryExpression("=|(!=)".r, comparison)
//  def comparison: Parser[Value] = binaryExpression("(<=)|(>=)|<|>".r, addition)
//  def addition: Parser[Value] = binaryExpression("\\+|\\-".r, multiplication)
//  def multiplication: Parser[Value] = binaryExpression("\\*|\\/|%".r, negative)
//  def negative: Parser[Value] = "\\-".r ~ power ^^ {
//    case operator ~ expression => Access(List(expression, Call(operator, List())))
//  } | power
//  def power: Parser[Value] = binaryExpression("\\*\\*".r, nonExpression)
//
//  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z\d_]*""".r ^^(string => Identifier(string))
//
//  def constant: Parser[Value] = float | int | bool | string | symbol | identifier
//  def nonExpression: Parser[Value] = access | call | block | dictionary | constant
//  def value: Parser[Value] = assignment | assignmentList | expression | function
//
//  def bool: Parser[ImplicitBoolean] = "true|false".r ^^ (string => ImplicitBoolean(string.toBoolean))
//  def int: Parser[ImplicitInt] = "[+-]?([1-9]\\d*)|0".r ^^ (string => ImplicitInt(string.toInt))
//  def float: Parser[ImplicitFloat] = """[+-]?((\d+\.\d*)|(\.\d+))(e[+-]?([1-9]\d*)|0)?""".r ^^ {
//    string => ImplicitFloat(string.toFloat)
//  }
//  def string: Parser[ImplicitString] = "\"".r ~> """((\\[\\nr"])|[^"])*""".r <~ "\"".r ^^ {
//    string => ImplicitString(replaceEscapes(string))
//  }
//  def symbol: Parser[ImplicitSymbol] = ":".r ~> identifier ^^ (string => ImplicitSymbol(string.name))
//
//  private def replaceEscapes(string: String) = {
//    val escapes = List(("""\\\\""", "\\\\"), ("""\\n""", "\n"), ("""\\r""", "\r"), ("""\\"""", "\""))
//    escapes.foldLeft(string) ((string, escape) => string.replaceAll(escape._1, escape._2))
//  }
//
//  def parse(code: String): List[Statement] = parseAll(program, removeSpaces(code)) match {
//    case Success(tree, _) => tree.filterNot(statement => statement == EmptyLine)
//    case Failure(message, _) => throw new Exception(message)
//  }
//
//  private def removeSpaces(code: String): String = code.split("\n").
//    map(_.replaceAll("\\s", "")).mkString("\n")
//}
