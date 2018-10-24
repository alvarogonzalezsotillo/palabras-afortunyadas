package rne

import scala.concurrent.Promise
import scala.concurrent.duration
import scala.concurrent.duration._
import scala.concurrent.Await


import scala.scalajs.js.JSApp
import io.scalajs.nodejs.fs.Fs
import io.scalajs.nodejs.fs.FileInputOptions
import io.scalajs.nodejs.FileIOError


class LineIterator( file: String, encoding: String = "latin1" ) extends scala.collection.Iterator[String]{

  def fileContents( file: String, encoding: String = "latin1" )(callback: (String) => Unit ) = {
    Fs.readFile(file, encoding, (err:FileIOError,data:String) => {
      callback(data)
    })
  }


  println( "FICHERO A ABRIR:" + file )

  var _data : Option[String] = None

  fileContents(file,encoding){ data =>
    _data = Some(data);
  }


  def hasNext: Boolean = true
  def next(): String = _data.get
}


object Main extends JSApp {

  def main(){
    Fs.readFile("./CREA_total.TXT", "latin1", (err:FileIOError,data:String) => println(data) )

    //PalabrasAnagramadas.resuelve()
  }

}


