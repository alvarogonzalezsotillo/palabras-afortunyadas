package rne

import scala.concurrent.Promise
import scala.concurrent.duration
import scala.concurrent.duration._
import scala.concurrent.Await


import scala.scalajs.js.annotation._
import scala.scalajs.js.JSApp
import scala.scalajs.js


import org.scalajs.dom
import dom.document


@JSExportTopLevel("Main")
object Main extends JSApp {

  def isBrowserPage = !js.isUndefined(document)
  def isBrowserWorker = !js.isUndefined(js.Dynamic.global.importScripts)
  def isNode = !isBrowserPage && !isBrowserWorker


  def fileContents( file: String, encoding: String = "latin1" )(callback: (String) => Unit ) = {

    if( isNode ) {
      /*
      import io.scalajs.nodejs.fs.Fs
      import io.scalajs.nodejs.fs.FileInputOptions
      import io.scalajs.nodejs.FileIOError

      Fs.readFile(file, encoding, (err:FileIOError,data:String) => {
        callback(data)
      })
      
      throw new Error("Para node, ModuleKind.CommonJSModule")
       */
      false
    }

    else{
      import org.scalajs.dom

      val xhr = new dom.XMLHttpRequest()
      xhr.open("GET",file)
      xhr.onload = { (e: dom.Event) =>

        if (xhr.status == 200) {
          callback( xhr.responseText )
        }
        else{
          println( s"Estado inesperado cargando '$file': ${xhr.status}")
        }
      }
      xhr.onerror = { (e: dom.Event) =>
          println( s"Error inesperado cargando '$file': ${xhr.status}")
      }
      xhr.send()
    }
  }

  def cargaCorpusJSON( file: String )( callback: (Corpus.Corpus) => Unit ) = {
    import scala.scalajs.js

    println( s"Voy a cargar el corpus:$file")

    fileContents( file, "utf8" ){ json =>
      val data = scala.scalajs.js.JSON.parse(json)
      val jsarray = data.asInstanceOf[js.Array[js.Array[String]]]
      val array : Array[Array[String]] = jsarray.toArray.map( _.toArray )
      val corpus = Corpus.palabras(array)
      println( s"Cargado el corpus: $corpus")
      callback(corpus)
    }
  }

 

  @JSExport
  def main(){
    import Message._


    if( isNode ){
      println( "Desde nodejs" )
    }

    if( isBrowserPage ){
      println( "Desde la página" )

      BrowserMain.main()
    }

    if( isBrowserWorker ){
      println( "Desde el worker" )
      WorkerMain.main()
    }
  }
}


