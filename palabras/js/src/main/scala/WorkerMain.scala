package rne

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport

@js.native
object WorkerGlobal extends js.GlobalScope {
  def addEventListener(`type`: String, f: js.Function): Unit = js.native
  def postMessage(data: js.Any): Unit = js.native
}

@JSExportTopLevel("WorkerMain")
object WorkerMain {


  
  implicit var corpus : Corpus.Corpus = null

  @JSExport
  def main(): Unit = {
    println( "WorkerMain.main")
    WorkerGlobal.addEventListener("message", onMessage _ )
    Main.cargaCorpusJSON("./corpus.json"){ c =>
      corpus = c
      WorkerGlobal.postMessage(s"Corpus cargado")
    }
  }

  def onMessage(msg: dom.MessageEvent) = {
    println( "WorkkerMain.onMessage:" + msg.data )

    msg.data match{
      /*
      case LoadCorpus(file) =>
        println( s"  worker: carga el corpus: $file")

      case SearchAnagram(s) =>
        println( s"  worker: busca anagrama: $s" )

        val coincidencias = PalabrasAnagramadas.buscaCoincidenciaExacta( Corpus.Palabra(s) );

        val ret = js.Array[String]()

        for( i <- 0 until coincidencias.size ){
          val p = coincidencias(i)
          ret += p.original
        }
        WorkerGlobal.postMessage( ret )
       */
      case data =>
        println( s"  worker: me llega algo que no sé lo que es: $data" )
        js.Dynamic.global.console.log(data.asInstanceOf[js.Any])

    }
  }
}
