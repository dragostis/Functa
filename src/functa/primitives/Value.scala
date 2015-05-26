package functa.primitives

class Value(val fileName: Option[String], val position: (Int, Int)) {
  private val definitions = Map[String, Value]()
  private val nativeMethods = Map[String, (List[Value], Map[String, Value]) => Value]()

  def access(name: String) = definitions.get(name)

  def native(name: String) = nativeMethods.get(name)

  def evaluate = this

  override def toString = "value"
}