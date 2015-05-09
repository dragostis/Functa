package primitives

class Block(getStatements: (Block) => List[Value], fileName: Option[String], position: (Int, Int),
            private val parent: Option[Block] = None) extends Value(fileName, position) {
  private val values     = scala.collection.mutable.Map[String, Value]()
  private val statements = getStatements(this)

  def getValue(name: String): Option[Value] = {
    values.get(name) match {
      case Some(value) => Option(value)
      case None        => parent match {
        case Some(block) => block.access(name)
        case None        => None
      }
    }
  }

  def evaluate = {

  }
}
