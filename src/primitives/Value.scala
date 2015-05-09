package primitives

class Value(val fileName: Option[String], val position: (Int, Int)) {
  val definitions = Map[String, Value]()

  def access(name: String): Option[Value] = definitions.get(name)
}