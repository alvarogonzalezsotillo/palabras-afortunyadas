object PalabrasAnagramadas extends App{

  import java.io._
  import java.util._

  val palabras = {
    val palabrasFile = "./CREA_total.TXT"


    val iterator = new scala.collection.Iterator[String]{

      def log(s:String) = () //println(s)

      val scanner = new Scanner( new FileInputStream(palabrasFile), "latin1" )
      val regex = """(?:\s*)(?:(\d|\.)*)(?:\s*)(\S*).*""".r


      def hasNext = scanner.hasNextLine()
      def next = {
        val line = scanner.nextLine()
        log( line )
        val group = regex.findAllMatchIn(line).next.subgroups(1)
        val ret = group
        log( "  --> " + ret )
        ret
      }
    }

    iterator.toArray
  }

  for( p <- palabras ){
    println(p)
  }

}

PalabrasAnagramadas.main( new Array(0) )

