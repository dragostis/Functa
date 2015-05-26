package functa.primitives

import functa.RuntimeError

class Call(name: String, arguments: List[Value], defaults: Map[String, Value], caller: Value, fileName: Option[String],
           position: (Int, Int)) extends Value(fileName, position) {
  override def evaluate = {
    caller.access(name) match {
      case Some(value) => value match {
        case function: Function => {
          function.check(arguments, defaults) match {
            case Some(message) => throw new RuntimeError(message, fileName, position)
            case None => {
              function(arguments, defaults)
              function.evaluate
            }
          }
        }
        case value => {
          if (!arguments.isEmpty && !defaults.isEmpty) throw new RuntimeError("Access does not take any arguments.",
            fileName, position)

          value
        }
      }
      case None => {
        caller.native(name) match {
          case Some(function) => function(arguments, defaults)
          case None => throw new RuntimeError(s"$name is not defined", fileName, position)
        }
      }
    }
  }
}
