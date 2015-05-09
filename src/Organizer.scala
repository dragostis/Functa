import primitives._

import scala.util.{Try, Success}

class Organizer(private val parser: Parser, private val fileName: Option[String]) {
  def getValue(value: parser.Value, block: Block): Value = {
    val fileName = value.fileName
    val position = value.position
    
    value match {
      case parser.BooleanLiteral(value)                => new FBoolean(value.toBoolean, fileName, position)
      case parser.IntegerLiteral(value)                => new FInteger(value.toInt, fileName, position)
      case parser.FloatLiteral(value)                  => new FFloat(value.toFloat, fileName, position)
      case parser.StringLiteral(value)                 => new FString(replaceEscapes(value), fileName, position)
      case parser.SymbolLiteral(value)                 => new Symbol(value, fileName, position)
      case assignment: parser.Assignment               => new Assignment(getAssignment(assignment, block), fileName,
                                                                         position)
      case parser.Call(name, values)                   => new Call(name, values.map(getValue(_, block)).toList, block,
                                                                   fileName, position)
      case parser.Access(value, calls)                 => calls match {
        case head :: tail => {
          val call = head match {
            case parser.Call(name, values) => new Call(name, values.map(getValue(_, block)).toList,
                                                       getValue(value, block), fileName, position)
          }

          tail.foldLeft(call) {
            case (call, parser.Call(name, values)) => new Call(name, values.map(getValue(_, block)).toList, call,
                                                               fileName, position)
          }
        }
      }
      case parser.Block(statements)                    => new Block(getBlockStatements(statements), fileName,
                                                                    position, Option(block))
      case parser.Dictionary(assignments)              => new Dictionary(getAssignmentMap(assignments, block),
                                                                         fileName, position)
      case parser.Function(arguments, defaults, value) => {
        val assignmentMap = getAssignmentMap(defaults, block)

        arguments match {
          case Some(arguments) => new Function(arguments.toList, assignmentMap, getValue(value, block), fileName,
                                               position)
          case None            => new Function(List(), assignmentMap, getValue(value, block), fileName, position)
        }
      }
    }
  }

  private def replaceEscapes(string: String) = {
    val escapes = List(("""\\\\""", "\\\\"), ("""\\n""", "\n"), ("""\\r""", "\r"), ("""\\"""", "\""))
    escapes.foldLeft(string) ((string, escape) => string.replaceAll(escape._1, escape._2))
  }

  def getAssignment(assignment: parser.Assignment, block: Block) = {
    val difference = assignment.names.length - assignment.values.length

    val errorMessage = difference match {
      case 0 => None
      case _ => {
        val plural = if (difference.abs > 1) "s" else ""

        if (difference > 0) Some(s"Left hand has $difference extra identifier$plural")
        else                Some(s"Right hand has $difference extra value$plural")
      }
    }

    errorMessage match {
      case Some(message) => throw new RuntimeError(message, assignment.fileName, assignment.position)
      case None          => {
        assignment.names.zip(assignment.values.map(getValue(_, block))).toList
      }
    }
  }

  def getBlockStatements(statements: Seq[parser.Value])(block: Block) = statements.map {
    getValue(_, block)
  }.toList

  def getAssignmentMap(assignments: Seq[parser.Assignment], block: Block) = {
    assignments.map(getAssignment(_, block).toMap).reduceLeft(_ ++ _)
  }

  def getValues = {
    parser.program.run() match {
      case Success(values) => Try(new Block(getBlockStatements(values), fileName, (0, 0), None))
      case failure         => failure
    }
  }
}
