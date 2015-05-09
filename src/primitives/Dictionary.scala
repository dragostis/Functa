package primitives

class Dictionary(override val definitions: Map[String, Value], fileName: Option[String], position: (Int, Int)) extends
  Value(fileName, position) {
}