package rne


class LineIterator(file: String, encoding : String = "latin1") extends scala.collection.Iterator[String]{
  import java.io._
  import java.util._

  val scanner = new Scanner(new FileInputStream(file), encoding)
  def hasNext: Boolean = scanner.hasNext
  def next(): String = scanner.next
}

object Main extends App{
  val li = new LineIterator("./CREA_total.TXT")
  val corpus = Corpus.palabras(li)
  PalabrasAnagramadas.resuelve(corpus)
}


