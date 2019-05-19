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

object CorpusSerializer{
  import Corpus._
  import java.io._


  def apply( corpus: Corpus, f: File ) : Unit = {
    def fos = new FileOutputStream(f)
    apply(corpus,fos)
  }

  def apply( corpus: Corpus, os: OutputStream ) : Unit = {
    val oos = new ObjectOutputStream(os)
    oos.writeObject(corpus)
    oos.writeObject("Done")
    oos.flush()
    oos.close()
  }

  def from( f: File ) = Cronometro.cronometro("Lectura de corpus desde fichero serializado: "){
    val fis = new FileInputStream(f)
    val ois = new ObjectInputStream(fis){
      override def resolveClass(desc: ObjectStreamClass) : Class[_] = {
        // Sobreescrito por problemas de classloader
        super.resolveClass(desc)
      }
    }
    val ret = ois.readObject()
    fis.close()
    ret.asInstanceOf[Corpus]
  }

  def computeOrReadCorpus( txt: String, cached: String ) : Corpus = Cronometro.cronometro("Corpus calculado"){
    val fcached = new File(cached)
    if( false && fcached.exists() ){
      // Es más rápido desde el fichero de texto, no sé por qué
      from(fcached)
    }
    else{
      val li = new LineIterator(txt)
      val corpus = Corpus.palabras(li)
      CorpusSerializer(corpus, fcached)
      corpus
    }
  }
}

object Main extends App{
  import java.io.File

  // val li = new LineIterator("./CREA_total.TXT")
  // val corpus = Corpus.palabras(li)
  // CorpusToJSon(corpus, new File("./corpus.json" ) )
  // CorpusSerializer(corpus, new File("./corpus.serialized"))
  val _ = Corpus.Palabra("a")
  val corpus = CorpusSerializer.computeOrReadCorpus( "./CREA_total.TXT", "./corpus.serialized")
  PalabrasAnagramadas.resuelve(corpus)
}


