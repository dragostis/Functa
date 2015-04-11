import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
//    FunctaParser.parse(scala.io.Source.fromFile("testing.fta").mkString)

    val tries = scala.io.Source.fromFile("testing.new").mkString;

    new FunctaParser(tries).program.run() match {
      case Success(i) => println(i)
      case Failure(e: ParseError) => println(e.format(tries))
    }
  }
}
