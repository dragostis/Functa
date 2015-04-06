import scala.util.parsing.combinator.RegexParsers

trait Statement

object EmptyLine extends Statement

case class Assignment(identifier: Identifier, value: Value) extends Statement
case class AssignmentList(assignments: List[Assignment]) extends Statement

case class ValueList(values: List[Value]) extends Value
case class Call(name: Value, values: List[Value]) extends Value

trait Value extends Statement

case class Access(values: List[Value]) extends Value

case class InputList(
  arguments: List[Identifier] = List(),
  defaults: List[Assignment] = List(),
  expandedArguments: Option[Identifier] = None,
  expandedDefaults: Option[Identifier] = None
)
case class Function(argLists: List[InputList], value: Value) extends Value

case class Block(statements: List[Statement]) extends Value
case class Dictionary(assignments: List[Assignment]) extends Value

case class Identifier(name: String) extends Value

case class ImplicitBoolean(value: Boolean) extends Value
case class ImplicitInt(value: Int) extends Value
case class ImplicitFloat(value: Float) extends Value
case class ImplicitString(value: String) extends Value
case class ImplicitSymbol(value: String) extends Value


object FunctaParser extends RegexParsers {
  override val skipWhitespace = false

  def emptyLine: Parser[Statement] = "".r ^^ (_ => EmptyLine)
  def comment: Parser[String] = "#".r ~> ".*".r

  def separator: Parser[String] = ",".r
  def statementSeparator: Parser[String] = """\n|(\n\s*\n)|;""".r

  def program: Parser[List[Statement]] = repsep(statement, statementSeparator)

  def statement: Parser[Statement] =  (assignment | assignmentList | access | value | emptyLine) <~ (comment ?)

  def access: Parser[Access] = (call | identifier) ~ "\\.".r ~ rep1sep(call | identifier, "\\.".r) ^^ {
    case head ~ _ ~ tail => Access(head :: tail)
  }

  def assignment: Parser[Assignment] = identifier ~ ":".r ~ value ^^ {
    case identifier ~ _ ~ value => Assignment(identifier, value)
  }
  def assignmentList: Parser[AssignmentList] = repsep(identifier, separator) ~ ":".r ~ repsep(value, separator) ^^ {
    case identifiers ~ _ ~ values => AssignmentList(identifiers.zip(values).map {
      case (identifier, value) => Assignment(identifier, value)
    })
  }

  def valueList: Parser[ValueList] = "\\(".r ~> repsep(value, separator) <~ "\\)".r ^^ (list => ValueList(list))
  def values: Parser[List[Value]] = rep1sep(valueList | block | dictionary, separator)

  def call: Parser[Call] = identifier ~ values ^^ {
    case identifier ~ values => Call(identifier, values)
  }

  def expandedArguments: Parser[Identifier] = "\\*".r ~> identifier
  def expandedDefaults: Parser[Identifier] = "&".r ~> identifier

  def explicitArguments: Parser[List[Identifier]] = rep1sep(identifier, separator)
  def explicitDefaults: Parser[List[Assignment]] = rep1sep(assignment, separator)

  def explicitOnly[T](explicitParser: => Parser[List[T]]): Parser[(List[T], Option[Identifier])] = {
    explicitParser ^^ (list => (list, None))
  }
  def expandedOnly[T](expandedParser: => Parser[Identifier]): Parser[(List[T], Option[Identifier])] = {
    expandedParser ^^ (identifier => (List(), Some(identifier)))
  }
  def explicitAndExpanded[T](
    explicitParser: => Parser[List[T]],
    expandedParser: => Parser[Identifier]
  ): Parser[(List[T], Option[Identifier])] = {
    explicitParser ~ separator ~ expandedParser ^^ {
      case list ~ _ ~ identifier => (list, Some(identifier))
    }
  }

  def arguments: Parser[(List[Identifier], Option[Identifier])] = {
    explicitAndExpanded(explicitArguments, expandedArguments) |
    explicitOnly(explicitArguments) |
    expandedOnly[Identifier](expandedArguments)
  }
  def defaults: Parser[(List[Assignment], Option[Identifier])] = {
    explicitAndExpanded(explicitDefaults, expandedDefaults) |
    explicitOnly(explicitDefaults) |
    expandedOnly[Assignment](expandedDefaults)
  }

  def argumentsOnly: Parser[InputList] = arguments ^^ {
    case (arguments, expandedArguments) => InputList(arguments = arguments, expandedArguments = expandedArguments)
  }
  def defaultsOnly: Parser[InputList] = defaults ^^ {
    case (defaults, expandedDefaults) => InputList(defaults = defaults, expandedDefaults = expandedDefaults)
  }
  def argumentsList: Parser[InputList] = "\\(".r ~> (argumentsOnly ?) <~ "\\)".r ^^ {
    case Some(inputList) => inputList
    case None => InputList()
  }
  def defaultsList: Parser[InputList] = "\\[".r ~> (defaultsOnly ?) <~ "\\]".r ^^ {
    case Some(inputList) => inputList
    case None => InputList()
  }

  def inputs: Parser[List[InputList]] = rep1sep(argumentsList | defaultsList, separator)

  def function: Parser[Function] = inputs ~ "=>".r ~ value ^^ {
    case arguments ~ _ ~ value => Function(arguments, value)
  }

  def block: Parser[Block] = "\\{\n*".r ~> rep1sep(statement, statementSeparator) <~  "\n*\\}".r ^^ {
    statements => Block(statements)
  }
  def dictionary: Parser[Dictionary] = "\\[\n*".r ~> rep1sep(assignment, """\n|(\n\s*\n)|,""".r) <~  "\n*\\]".r ^^ {
    assignment => Dictionary(assignment)
  }

  def identifier: Parser[Identifier] = """\w[\w\d_]*""".r ^^(string => Identifier(string))

  def value: Parser[Value] = call | block | dictionary | function | float | int | bool | string | symbol | identifier

  def numeric: Parser[Value] = float | int

  def bool: Parser[ImplicitBoolean] = "true|false".r ^^ (string => ImplicitBoolean(string.toBoolean))
  def int: Parser[ImplicitInt] = "[+-]?[1-9]\\d*".r ^^ (string => ImplicitInt(string.toInt))
  def float: Parser[ImplicitFloat] = """[+-]?(\d+\.\d*)|(\.\d+)""".r ^^ (string => ImplicitFloat(string.toFloat))
  def string: Parser[ImplicitString] = "\"".r ~> """[^\"]*""".r <~ "\"".r ^^ (string => ImplicitString(string))
  def symbol: Parser[ImplicitSymbol] = ":".r ~> identifier ^^ (string => ImplicitSymbol(string.name))

  def parse(code: String): List[Statement] = parseAll(program, removeSpaces(code)) match {
    case Success(tree, _) => tree.filterNot(statement => statement == EmptyLine)
    case Failure(message, _) => throw new Exception(message)
  }

  private def removeSpaces(code: String): String = code.split("\n").
    map(_.replaceAll("\\s", "")).mkString("\n")
}
