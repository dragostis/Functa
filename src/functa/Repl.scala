package functa

import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object Repl {
  def main(args: Array[String]): Unit = {
    def repl: Unit = {
      val line = readLine("functa> ")

      new Parser(line).program.run() match {
        case Success(i) => println(i)
        case Failure(e: ParseError) => println(e.format(line))
      }

      println()

      repl
    }

    repl
  }
}
