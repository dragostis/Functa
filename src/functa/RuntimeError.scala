package functa

class RuntimeError(message: String, fileName: Option[String], position: (Int, Int)) extends RuntimeException {
  def format = message + (position match {
    case (line, column) => s" (line $line, column $column)"
  }) + (fileName match {
    case Some(fileName) => s" in file $fileName"
    case None => ""
  })
}
