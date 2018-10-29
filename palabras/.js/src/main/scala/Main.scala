package rne

import scala.concurrent.Promise
import scala.concurrent.duration
import scala.concurrent.duration._
import scala.concurrent.Await


import scala.scalajs.js.annotation._
import scala.scalajs.js.JSApp

/*
import io.scalajs.nodejs.fs.Fs
import io.scalajs.nodejs.fs.FileInputOptions
import io.scalajs.nodejs.FileIOError
*/


@JSExportTopLevel("Main")
object Main extends JSApp {

  class RegexLineIterator( data: String ) extends scala.collection.Iterator[String]{

    val lineRegExp = """(.*)(\r\n|\r|\n)""".r

    val lines = lineRegExp.findAllMatchIn(data)


    def hasNext: Boolean = lines.hasNext
    def next(): String = lines.next.group(1)
  }

  class ManualLineIterator( data: String ) extends scala.collection.Iterator[String]{

    def findNextEndOfLine(from:Int) =  data.indexWhere( _ == '\n', from )

    private var ini : Int = 0
    private var end : Int = findNextEndOfLine(0)

    def hasNext: Boolean = end > ini
    def next(): String = {
      val ret = data.slice(ini,end)
      ini = end+1
      end = findNextEndOfLine(ini)
      ret
    }
  }


  def isBrowser = {
    import org.scalajs.dom
    import dom.document
    import dom.window
    import scala.util.Try

    Try{
      new dom.XMLHttpRequest()
    }.isSuccess
  }

  def isNode = !isBrowser


  def fileContents( file: String, encoding: String = "latin1" )(callback: (String) => Unit ) = {

    if( isNode ) {
      /*
      Fs.readFile(file, encoding, (err:FileIOError,data:String) => {
        callback(data)
      })
       */
      throw new Error("No sé hacer a la vez algo para el browser y para nodejs")
    }

    else{
      import org.scalajs.dom

      val xhr = new dom.XMLHttpRequest()
      xhr.open("GET",file)
      xhr.onload = { (e: dom.Event) =>

        if (xhr.status == 200) {
          callback( xhr.responseText )
        }
      }
      xhr.send()
    }
  }

  private var corpus : Corpus.Corpus = null

  def cargaCorpus( file: String )( callback: (Corpus.Corpus) => Unit ) = {
    fileContents( file ){ data =>
      println( s"File '$file' readed")
      val li = new ManualLineIterator(data)
      corpus = Corpus.palabras(li)
      callback(corpus)
    }
  }

  def ejecutaPrueba() = {
    cargaCorpus( "./corpus-100000.txt")(PalabrasAnagramadas.resuelve(_))
  }


  @JSExport
  def main(){
    println( "Main ejecutado")
  }
}


