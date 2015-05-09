package primitives

class Assignment(assignments: List[(String, Value)], fileName: Option[String], position: (Int, Int)) extends
  Value(fileName, position)
