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


  import Message._
  
  implicit var corpus : Corpus.Corpus = null

  @JSExport
  def main(): Unit = {
    println( "WorkerMain.main")
    WorkerGlobal.addEventListener("message", onMessage _ )
  }

  def preparseCorpus() = {
    for( size <- 1 to 15 ){
        println( s"  worker: preparse: $size")
        PalabrasAnagramadas.buscaCoincidenciaExacta( Corpus.Palabra("a"*size) ).foreach( c => c )
        WorkerGlobal.postMessage( PreparseDone(size) )
    }
  }

  def onMessage(msg: dom.MessageEvent) = {
    println( "WorkkerMain.onMessage:" + msg.data )

    msg.data match{
      case LoadCorpus(file) =>
        println( s"  worker: carga el corpus: $file")
        Main.cargaCorpusJSON(file){ c =>
          corpus = c
          //preparseCorpus(c);
          WorkerGlobal.postMessage( CorpusLoaded(file) )
        }


      case SearchAnagram(s) =>
        println( s"  worker: busca anagrama: $s" )

        val coincidencias = PalabrasAnagramadas.buscaCoincidenciaExacta( Corpus.Palabra(s) );

        for( c <- coincidencias ){
          WorkerGlobal.postMessage( AnagramFound(c.original,s) )
        }

        WorkerGlobal.postMessage( NoMoreAnagrams(s) )

      case SearchAnagramInSentence(s,size) =>
        println( s"  worker: busca anagrama en frase: $s: $size" )

        val coincidencias = PalabrasAnagramadas.buscaExactoEnFrase( s, size );

        for( c <- coincidencias ){
          WorkerGlobal.postMessage( AnagramFound(c.original,s) )
        }

        WorkerGlobal.postMessage( NoMoreAnagrams(s) )


      case data =>
        println( s"  worker: me llega algo que no sé lo que es: $data" )
        js.Dynamic.global.console.log(data.asInstanceOf[js.Any])

    }
  }
}
