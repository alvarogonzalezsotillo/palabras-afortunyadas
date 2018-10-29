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
    Main.cargaCorpus("./corpus-100000.txt"){ c =>
      corpus = c
      WorkerGlobal.postMessage(s"Corpus cargado")
    }
  }

  def onMessage(msg: dom.MessageEvent) = {
    println( "WorkkerMain.onMessage:" + msg.data )
    val s = msg.data.asInstanceOf[String]

    val coincidencias = PalabrasAnagramadas.buscaCoincidenciaExacta( Corpus.Palabra(s) );

    WorkerGlobal.postMessage( coincidencias.mkString(",") )


  }
}
