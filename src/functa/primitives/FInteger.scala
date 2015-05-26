package functa.primitives

class FInteger(value: Int, fileName: Option[String], position: (Int, Int)) extends Value(fileName, position) {
  override def toString = value.toString
}
