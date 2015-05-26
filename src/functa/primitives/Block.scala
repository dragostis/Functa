package functa.primitives

class Block(getStatements: (Block) => List[Value], fileName: Option[String], position: (Int, Int),
            private val parent: Option[Block] = None) extends Value(fileName, position) {
  private val values = scala.collection.mutable.Map[String, Value]()
  private val statements = getStatements(this)

  def getValue(name: String) = {
    values.get(name) match {
      case Some(value) => Option(value)
      case None => parent match {
        case Some(block) => block.access(name)
        case None => None
      }
    }
  }

  def setValue(name: String, value: Value) = values(name) = value

  def setValues(values: Map[String, Value]) = values ++ values

  def clear = values.clear

  override def evaluate = {
    statements.foldLeft(statements.head)((_, next) => next.evaluate)
  }
}
