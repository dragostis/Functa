package functa.primitives

class Symbol(value: String, fileName: Option[String], position: (Int, Int)) extends Value(fileName, position)
