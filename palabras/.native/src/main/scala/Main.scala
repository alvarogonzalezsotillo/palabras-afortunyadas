package rne


class LineIterator(file: String, encoding : String = "latin1") extends scala.collection.Iterator[String]{
  import java.io._
  import java.util._

  val reader = new BufferedReader( new InputStreamReader( new FileInputStream(file), encoding ) )


  var ret = ""
  def hasNext: Boolean = ret != null
  def next(): String = {
    ret = reader.readLine()
    ret
  }
}

object Main extends App{
  
  import java.io.File

  val li = new LineIterator("./CREA_total.TXT")
  val corpus = Corpus.palabras(li)
  
  PalabrasAnagramadas.resuelve(corpus)
  

  println("Hola Native")
}


