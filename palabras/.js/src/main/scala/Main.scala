package rne

import scala.concurrent.Promise
import scala.concurrent.duration
import scala.concurrent.duration._
import scala.concurrent.Await


import scala.scalajs.js.annotation._
import scala.scalajs.js.JSApp
import io.scalajs.nodejs.fs.Fs
import io.scalajs.nodejs.fs.FileInputOptions
import io.scalajs.nodejs.FileIOError



@JSExportTopLevel("Main")
object Main extends JSApp {

  class LineIterator( data: String ) extends scala.collection.Iterator[String]{

    val lineRegExp = """(.*)(\r\n|\r|\n)""".r

    val lines = lineRegExp.findAllMatchIn(data)


    def hasNext: Boolean = lines.hasNext
    def next(): String = lines.next.group(1)
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
      Fs.readFile(file, encoding, (err:FileIOError,data:String) => {
        callback(data)
      })
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


  @JSExport
  def main(){

    println( s"isBrowser:$isBrowser")
    fileContents( "./CREA_total.TXT"){ data =>
      println( "File readed")
      val li = new LineIterator(data)
      val corpus = Corpus.palabras(li)
      PalabrasAnagramadas.resuelve(corpus)

    }
  }

}


