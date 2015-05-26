package functa

import org.parboiled2._

class Parser(val input: ParserInput, parsedFileName: Option[String] = None) extends org.parboiled2.Parser {
  implicit def index = super.cursor

  class Value(implicit private val index: Int) {
    val fileName = parsedFileName
    val position = {
      val position = Position(index - 1, input)
      (position.line, position.column)
    }
  }

  case class Assignment(names: Seq[String], values: Seq[Value]) extends Value
  case class Default(name: String, value: Value)

  case class Call(name: String, values: Option[Seq[Value]], defaults: Seq[Assignment]) extends Value

  case class Access(value: Value, calls: Seq[Call]) extends Value

  case class Function(arguments: Option[Seq[String]], defaults: Seq[Assignment], value: Value) extends Value

  case class FList(values: Seq[Value]) extends Value
  case class Block(statements: Seq[Value]) extends Value
  case class Dictionary(assignments: Seq[Assignment]) extends Value

  case class BooleanLiteral(value: String) extends Value
  case class IntegerLiteral(value: String) extends Value
  case class FloatLiteral(value: String) extends Value
  case class StringLiteral(value: String) extends Value
  case class SymbolLiteral(value: String) extends Value

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
  def dictionary = rule('{' ~ assignments.named("assignment") ~ '}' ~> Dictionary)
  def list       = rule('[' ~ (values.named("value") ?) ~ ']' ~> {
    values => values match {
      case Some(values) => FList(values)
      case None         => FList(Seq.empty)
    }
  })

  def default  = rule(capture(identifier) ~ quiet(space) ~ '~' ~ quiet(breakableSpace) ~ value ~> {
    (name, value) => Assignment(Seq(name), Seq(value))
  })
  def defaults = rule(quiet(breakableSpace) ~ ((default ~ quiet(space)) *).separatedBy(quiet(elementSeparator)) ~
    quiet(breakableSpace))

  def emptyCall     = rule(capture(identifier) ~> (identifier => Call(identifier, None, Seq.empty)))
  def parenCall     = rule(capture(identifier) ~ '(' ~ quiet(breakableSpace) ~ callArgs.named("arguments") ~
    quiet(breakableSpace) ~ ')' ~> Call)
  def parenlessCall = rule(capture(identifier) ~ quiet(whiteSpace) ~ quiet(space) ~ callArgs.named("arguments") ~>
    Call)
  def callValues    = rule(((value ~ quiet(space) ~ !'~') +).separatedBy(quiet(elementSeparator)))
  def callArgs      = rule(
    (callValues ~ quiet(space) ~ ',' ~ quiet(breakableSpace) ?) ~ defaults ~ quiet(space) ~ !value |
    (callValues ~ quiet(space) ?) ~ quiet(space) ~ push(Seq.empty)
  )
  def call          = rule(!boolean ~ (parenCall.named("call") | parenlessCall.named("call") | emptyCall.named("call")))

  def access = rule(nonAccess ~ '.' ~ (call +).separatedBy('.') ~> Access)

  def arguments = rule(((capture(identifier) ~ quiet(space) ~ !'~') +).separatedBy(quiet(elementSeparator)))
  def inputs    = rule(
    (arguments ~ quiet(space) ~ ',' ~ quiet(breakableSpace) ?) ~ defaults ~ quiet(space) ~ !identifier |
    (arguments ~ quiet(space) ?) ~ quiet(space) ~ push(Seq.empty)
  )
  def function  = rule(inputs ~ "=>" ~ quiet(breakableSpace) ~ expression ~> Function)

  def parens = rule('(' ~ value ~ ')')

  def unary(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    ((capture(operator()) ~ quiet(!digit)) ?) ~ quiet(breakableSpace) ~ nextExpression() ~> {
      (operator, value) => operator match {
        case Some(operator) => Access(value, Seq(Call(operator, None, Seq.empty)))
        case None           => value
      }
    }
  }

  def leftSide(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    nextExpression() ~ quiet(space) ~ capture(operator()) ~ quiet(breakableSpace) ~
      &(nextExpression() ~ quiet(space) ~ capture(operator()) ~ quiet(breakableSpace)) ~> {
      (value, operator) => Call(operator, Some(Seq(value)), Seq.empty)
    }
  }
  def rightSide(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    quiet(space) ~ capture(operator()) ~ quiet(breakableSpace) ~ nextExpression() ~> {
      (operator, value) => Call(operator, Some(Seq(value)), Seq.empty)
    }
  }

  def leftBinary(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    binary(operator, nextExpression) ~ (rightSide(operator, nextExpression) *) ~> {
      (access, calls) => access match {
        case Access(value, call) => Access(value, call ++ calls)
        case value: Value        => value
      }
    }
  }
  def rightBinary(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    (leftSide(operator, nextExpression) *) ~ binary(operator, nextExpression) ~> {
      (calls, access) => calls.foldRight(access) {
        (call, access) => Access(call.values.get.head, Seq(Call(call.name, Some(Seq(access)), Seq.empty)))
      }
    }
  }
  def binary(operator: () => Rule0, nextExpression: () => Rule1[Value]) = rule {
    nextExpression() ~ (rightSide(operator, nextExpression) ?) ~> {
      (value, call) => call match {
        case Some(call) => Access(value, Seq(call))
        case None       => value
      }
    }
  }

  val rangeOperator          = () => rule("..." | "..")
  val disjunctionOperator    = () => rule("||")
  val conjunctionOperator    = () => rule("&&")
  val equalityOperator       = () => rule("!=" | "=")
  val comparisonOperator     = () => rule("<=" | ">=" | "<" | ">")
  val bitwiseOrOperator      = () => rule("|" | "^")
  val bitwiseAndOperator     = () => rule("&")
  val bitwiseShiftOperator   = () => rule("<<" | ">>")
  val additionOperator       = () => rule("+" | "-")
  val multiplicationOperator = () => rule("*" | "/" | "%")
  val negativeOperator       = () => rule("-")
  val exponentiationOperator = () => rule("**")
  val negationOperator       = () => rule("!")

  def range          = rule(binary(rangeOperator, disjunction))
  val disjunction    = () => rule(leftBinary(disjunctionOperator, conjunction))
  val conjunction    = () => rule(leftBinary(conjunctionOperator, equality))
  val equality       = () => rule(binary(equalityOperator, comparison))
  val comparison     = () => rule(binary(comparisonOperator, bitwiseOr))
  val bitwiseOr      = () => rule(leftBinary(bitwiseOrOperator, bitwiseAnd))
  val bitwiseAnd     = () => rule(leftBinary(bitwiseAndOperator, bitwiseShift))
  val bitwiseShift   = () => rule(leftBinary(bitwiseShiftOperator, addition))
  val addition       = () => rule(leftBinary(additionOperator, multiplication))
  val multiplication = () => rule(leftBinary(multiplicationOperator, negative))
  val negative       = () => rule(unary(negativeOperator, exponentiation))
  val exponentiation = () => rule(rightBinary(exponentiationOperator, negation))
  val negation       = () => rule(unary(negationOperator, nonExpression))
  val nonExpression  = () => rule(parens | access | nonAccess)

  def constant      = rule(floatLiteral | integerLiteral| booleanLiteral | stringLiteral | symbolLiteral)
  def nonAccess     = rule(call | dictionary | block | list | constant)
  def expression    = rule(range)

  def value: Rule1[Value] = rule(function | assignment | expression)
  def values              = rule(((value ~ quiet(space)) +).separatedBy(quiet(elementSeparator)))

  def integerLiteral = rule(capture(integer) ~> IntegerLiteral)
  def floatLiteral   = rule(capture(float)   ~> FloatLiteral)
  def booleanLiteral = rule(capture(boolean) ~> BooleanLiteral)
  def stringLiteral  = rule(capture(string)  ~> StringLiteral)
  def symbolLiteral  = rule(capture(symbol)  ~> SymbolLiteral)

  def identifier  = rule(letter ~ ((letter | digit | '_') *) | '_' ~ operator ~ '_')
  def identifiers = rule(((capture(identifier) ~ quiet(space)) +).separatedBy(quiet(elementSeparator)))

  def boolean = rule(quiet("true") | quiet("false"))
  def integer = rule(('-' ?) ~ (quiet(digit19) ~ oneOrMore(digit) | digit))
  def float   = rule(('-' ?) ~ ((digit +) ~ '.' ~ (digit +) | '.' ~ (digit +)) ~ (exponent ?))
  def string  = rule('"' ~ (('\\' ~ escapeCharacter | !'"' ~ !'\\' ~ ANY) *) ~ '"')
  def symbol  = rule(':' ~ ((letter ~ ((letter | digit | '_') *)) | operator))

  def exponent        = rule('e' ~ anyOf("+-") ~ (digit +))
  def escapeCharacter = rule(anyOf("\"\\nrtf"))
  
  def digit    = rule(Digit)
  def digit19  = rule(Digit19)
  def letter   = rule(Alpha)
  def operator = rule(
    "..." | ".." | "||" | "&&" | "!=" | "=" | "<<" | ">>" | "<=" | ">=" | "<" | ">" | "|" | "^" | "&" | "+" | "-" |
    "*" | "/" | "%" | "-" | "**" | "!"
  )
}