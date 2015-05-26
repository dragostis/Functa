package functa

import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val simple = scala.io.Source.fromFile("simple.fta").mkString;

    new Runtime(simple, Some("simple.fta")).evaluate match {
      case Success(i) => println(i)
      case Failure(e: ParseError) => println(e.format(simple))
    }
  }
}
