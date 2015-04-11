import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val tries = scala.io.Source.fromFile("testing.fta").mkString;

    new FunctaParser(tries).program.run() match {
      case Success(i) => println(i)
      case Failure(e: ParseError) => println(e.format(tries))
    }
  }
}
