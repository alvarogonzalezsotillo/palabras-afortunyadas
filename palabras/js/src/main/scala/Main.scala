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

  def ejecutaPruebaJSON() = {
    cargaCorpusJSON("./corpus.json"){ c =>
      println(c(4).mkString(","))
    }
 
  }


  def setupUI(): Unit = {
    import org.scalajs.jquery._
    import Message._

    jQuery("#output").text("Desde jquery")
    jQuery("#botonPalabra").click{ event : js.Any =>
      val palabra = jQuery("#palabra").value().toString
      println( s"Voy a enviar SearchAnagram($palabra)")
      worker.postMessage( SearchAnagram(palabra) )
    }
  }

  def lastLoadedScript() : String = {
    println( "Hay que implementar lastLoadedScript")
    "./palabras/js/target/scala-2.11/palabras-fastopt.js"
  }

  var worker : org.scalajs.dom.raw.Worker = null

  @JSExport
  def main(){
    import Message._



    if( isNode ){
      ejecutaPruebaJSON()
    }

    if( isBrowserPage ){
      println( "Desde la página" )

      import org.scalajs.jquery._
      jQuery(() => setupUI())
      worker = new org.scalajs.dom.raw.Worker(lastLoadedScript)
      
      worker.onmessage = (m : org.scalajs.dom.raw.MessageEvent) =>  {
        println( s"Mensaje recibido en html")
        js.Dynamic.global.console.log(m.data.asInstanceOf[js.Any])
        m.data match{
          case CorpusLoaded(_) => jQuery("#botonPalabra").prop("disabled",false)
          case data =>
            println( s"No entiendo el mensaje en html:$data")
            js.Dynamic.global.console.log(data.asInstanceOf[js.Any])
            
        }

      }

      val data = LoadCorpus("./corpus.json")
      println("Envio desde html: " + data )

      worker.postMessage( data )
    }

    if( isBrowserWorker ){
      println( "Desde el worker" )
      WorkerMain.main()
    }
  }
}


