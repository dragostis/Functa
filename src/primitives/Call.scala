package primitives

class Call(name: String, value: List[Value], caller: Value, fileName: Option[String], position: (Int, Int)) extends
  Value(fileName, position)
