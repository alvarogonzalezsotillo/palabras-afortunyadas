package rne


class LineIterator(file: String, encoding : String = "latin1") extends scala.collection.Iterator[String]{
  import java.io._
  import java.util._

  val scanner = new Scanner(new FileInputStream(file), encoding)
  def hasNext: Boolean = scanner.hasNext
  def next(): String = scanner.nextLine
}

object CorpusToJSon{

  import java.io.PrintStream
  import java.io.File
  import java.io.FileOutputStream
  import Corpus._



  def apply(corpus: Corpus, out: File ) : Unit = {
    val ps = new PrintStream( new FileOutputStream(out) )
    apply(corpus,ps)
    ps.close()
  }

  def apply(corpus: Corpus, out: PrintStream ) : Unit = {

    def stringArray( array: Array[Palabra]) = {
      out.println("  [")
      for( i <- 0 until array.size; comma = if(i==array.size-1) "" else "," ){
        out.println(s"""    "${array(i).original}"$comma""")
      }
      out.println("  ]")
    }

    out.println("[")
    for( l <- 1 to corpus.size ){
      val palabras = corpus.getOrElse(l,Array() )
      stringArray( palabras )
      if( l < corpus.size ){
        out.println("  ,")
      }
    }
    out.println("]")
  }

}


object Main extends App{
  import java.io.File

  val li = new LineIterator("./CREA_total.TXT")
  val corpus = Corpus.palabras(li)
  
  CorpusToJSon(corpus, new File("./corpus.json" ) )
  PalabrasAnagramadas.resuelve(corpus)
}


