package functa

import scala.util.{Try, Success}

class Runtime(input: String, fileName: Option[String] = None) {
  private val parser = new Parser(input, fileName)

  def evaluate = {
    new Organizer(parser, fileName).getBlock match {
      case Success(block) => Try(block.evaluate)
      case failure => failure
    }
  }
}
