package functa.primitives

class Dictionary(definitions: Map[String, Value], fileName: Option[String], position: (Int, Int)) extends
Value(fileName, position) {
  override def evaluate = {
    val evaluated = definitions.map {
      case (name, value) => (name, value.evaluate)
    }

    new Dictionary(evaluated, fileName, position)
  }

  override def toString = "{" + definitions.toList.map(pair => pair._1 + " : " + pair._2.toString).mkString(", ") + "}"
}