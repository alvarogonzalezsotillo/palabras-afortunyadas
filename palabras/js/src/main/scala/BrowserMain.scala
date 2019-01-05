package rne

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("BrowserMain")
object BrowserMain {

  import Message._
  import org.scalajs.jquery._

  def setupUI(): Unit = {

    ui.botonPalabra.click{ event : js.Any =>
      disableButtons()
      ui.botonPalabra.value("Buscando")
      val palabra = ui.palabra.value().toString
      println( s"Voy a enviar SearchAnagram($palabra)")
      ui.output.text("")
      worker.get.postMessage( SearchAnagram(palabra) )
    }

    ui.botonFrase.click{ event: js.Any =>
      disableButtons()
      ui.botonFrase.value("Buscando")
      val frase = ui.frase.value().toString
      val size = ui.selectFrase.value().toString.toInt
      println( s"Voy a enviar frase: $frase: $size")
      ui.output.text("")
      worker.get.postMessage( SearchAnagramInSentence(frase, size) )
    }
  }

  def addWord( word: String ) = {
    val e = jQuery( s"""<span class="word">$word</span>""")
    ui.output.append(e)
  }

  def addLog( msg: String ) = {
    val e = jQuery( s"""<span class="log">$msg</span>""")
    ui.output.append(e)
  }

  def disableButtons() = {
    ui.botonPalabra.prop("disabled", true)
    ui.botonFrase.prop("disabled", true)
  }

  def enableButtons() = {
    ui.botonPalabra.prop("disabled", false)
    ui.botonFrase.prop("disabled", false)
  }


  val currentScript = {
    val ret = js.Dynamic.global.document
    if( js.isUndefined(ret) ) None else Some(ret.currentScript)
  }

  val lastLoadedScript : Option[String] = currentScript.map{ c =>
    if( js.isUndefined(c) )
      "./palabras/js/target/scala-2.11/palabras-fastopt.js"
    else
      c.src.toString
  }
  


  val worker = lastLoadedScript.map( new org.scalajs.dom.raw.Worker(_) )

  object ui{
    val output = jQuery("#output")
    val botonPalabra = jQuery("#botonPalabra")
    val palabra = jQuery("#palabra")
    val frase = jQuery("#frase")
    val selectFrase = jQuery("#selectFrase")
    val botonFrase = jQuery("#botonFrase")

  }

  def onMessage( m: org.scalajs.dom.raw.MessageEvent ) = {
    println( s"Mensaje recibido en html")

    m.data match{
      case CorpusLoaded(_) =>
        enableButtons()
        ui.output.text("")

      case AnagramFound(found,_) =>
        addWord(found)

      case NoMoreAnagrams(s) =>
        enableButtons()
        ui.botonPalabra.value("Busca anagramas")
        ui.botonFrase.value("Busca anagramas en la frase")
        addLog( s"No se encuentran más anagramas para $s" )

      case data =>
        println( s"No entiendo el mensaje en html:$data")
        js.Dynamic.global.console.log(data.asInstanceOf[js.Any])
    }

  }

  @JSExport
  def main(): Unit = {
    import org.scalajs.jquery._
    worker.foreach{ w =>
      jQuery(() => setupUI())
      w.onmessage = onMessage _
      addLog( "Cargando corpus...")
      w.postMessage( LoadCorpus("./corpus.json") )
    }
  }
}
