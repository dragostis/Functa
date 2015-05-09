package primitives

class Function(arguments: List[String], defaults: Map[String, Value], value: Value, fileName: Option[String],
               position: (Int, Int)) extends Value(fileName, position)
