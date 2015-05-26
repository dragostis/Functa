package functa.primitives

class FList(values: List[Value], fileName: Option[String], position: (Int, Int)) extends Value(fileName, position) {
  override def evaluate = new FList(values.map(_.evaluate), fileName, position)
}