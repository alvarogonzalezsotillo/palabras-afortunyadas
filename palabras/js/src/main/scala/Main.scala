package rne

import scala.concurrent.Promise
import scala.concurrent.duration
import scala.concurrent.duration._
import scala.concurrent.Await


import scala.scalajs.js.annotation._
import scala.scalajs.js.JSApp

import org.scalajs.dom
import dom.document


@JSExportTopLevel("Main")
object Main extends JSApp {

  def isBrowserPage = !scala.scalajs.js.isUndefined(document)
  def isBrowserWorker = !scala.scalajs.js.isUndefined(scala.scalajs.js.Dynamic.global.importScripts)
  def isNode = !isBrowserPage && !isBrowserWorker


  def fileContents( file: String, encoding: String = "latin1" )(callback: (String) => Unit ) = {

    if( isNode ) {
      import io.scalajs.nodejs.fs.Fs
      import io.scalajs.nodejs.fs.FileInputOptions
      import io.scalajs.nodejs.FileIOError

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

  def cargaCorpusJSON( file: String )( callback: (Corpus.Corpus) => Unit ) = {
    import scala.scalajs.js
    fileContents( file, "utf8" ){ json =>
      val data = scala.scalajs.js.JSON.parse(json)
      val jsarray = data.asInstanceOf[js.Array[js.Array[String]]]
      val array : Array[Array[String]] = jsarray.toArray.map( _.toArray )
      val corpus = Corpus.palabras(array)
      callback(corpus)
    }
  }

  def ejecutaPruebaJSON() = {
    cargaCorpusJSON("./corpus.json"){ c =>
      println(c(4).mkString(","))
    }
 
  }


  def setupUI(): Unit = {
    import org.scalajs.jquery._
    jQuery("#output").text("Desde jquery")
  }

  @JSExport
  def main(){
    println( s"isNode:$isNode")
    println( s"isBrowserPage:$isBrowserPage")
    println( s"isBrowserWorker:$isBrowserWorker")
    println( scala.scalajs.js.isUndefined(WorkerGlobal.postMessage _ ) )

    if( isNode ){
      ejecutaPruebaJSON()
    }

    if( isBrowserPage ){
      import org.scalajs.jquery._
      jQuery(() => setupUI())
    }

    println( "Main ejecutado")
  }
}


