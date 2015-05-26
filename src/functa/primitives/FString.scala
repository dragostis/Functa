package functa.primitives

class FString(value: String, fileName: Option[String], position: (Int, Int)) extends Value(fileName, position)