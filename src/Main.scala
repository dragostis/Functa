object Main {
  def main(args: Array[String]): Unit = {
    FunctaParser.parse(scala.io.Source.fromFile("testing.fta").mkString)
  }
}
