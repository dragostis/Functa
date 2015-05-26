package functa.primitives

class Function(arguments: List[String], defaults: Map[String, Value], block: Block, fileName: Option[String],
               position: (Int, Int)) extends Value(fileName, position) {
  def check(arguments: List[Value], defaults: Map[String, Value]) = {
    val args = arguments.length
    val tArgs = this.arguments.length
    val dfts = defaults.size
    val tDfts = this.defaults.size

    if (args < tArgs) {
      Option(s"Too few arguments. ($args instead of $tArgs)")
    } else if (args > tArgs + tDfts) {
      Option(s"Too many arguments. ($args instead of ${tArgs + tDfts}")
    } else if (args + dfts > tArgs + tDfts) {
      Option(s"Too many arguments. (${args + dfts} instead of ${tArgs + tDfts}")
    } else {
      val notDefined = defaults.keys.filterNot(name => this.defaults.contains(name)).mkString(", ")

      if (notDefined != "") Option(s"Default arguments $notDefined are not defined.")
      else None
    }
  }

  def apply(arguments: List[Value], defaults: Map[String, Value]): Unit = {
    block.clear

    block.setValues(this.arguments.zip(arguments).toMap ++ this.defaults ++ defaults)
  }

  override def evaluate: Value = block.evaluate
}
