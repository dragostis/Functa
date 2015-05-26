package functa.primitives

class Assignment(assignments: List[(String, Value)], block: Block, fileName: Option[String],
                 position: (Int, Int)) extends Value(fileName, position) {
  override def evaluate = {
    val values = assignments.map {
      case (name, value) => {
        val evaluated = value.evaluate

        block.setValue(name, evaluated)

        evaluated
      }
    }

    values match {
      case List(value) => value
      case values => new FList(values, fileName, position)
    }
  }
}
