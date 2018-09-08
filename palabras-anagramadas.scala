
object PalabrasAnagramadas extends App {




  import scala.collection.immutable.Map
  import scala.collection.Iterator
  import java.io._
  import java.util._


  type Frase = Array[Palabra]
  type Histograma = Map[Char, Int]

  case class Palabra(p: String) {

    import scala.concurrent.ExecutionContext.Implicits.global

    private def quitaAcentos(s: String): String = {
      val acentos = Map(
        'á' -> 'a',
        'é' -> 'e',
        'í' -> 'i',
        'ó' -> 'o',
        'ú' -> 'u',
        'ü' -> 'u'
      )
      s.toLowerCase.map(c => if (acentos.isDefinedAt(c)) acentos(c) else c)
    }

    val size = p.size
    lazy val palabra = quitaAcentos(p)

    lazy val histograma : Histograma = {
      val ret = palabra.groupBy(c => c)
      ret.map { case (c, set) => (c, set.size) }
    }
  }


  val palabras: Map[Int, Array[Palabra]] = {

    val iterator = new Iterator[String] {

      val palabrasFile = "./CREA_total.TXT"

      val scanner = new Scanner(new FileInputStream(palabrasFile), "latin1")
      val regex = """(?:\s*)(?:(\d|\.)*)(?:\s*)(\S*).*""".r

      def hasNext = scanner.hasNextLine()

      def next = {
        val line = scanner.nextLine()
        regex.findAllMatchIn(line).next.subgroups(1)
      }
    }

    val todas = iterator.take(300000).map(p => Palabra(p)).toArray.sortBy(_.palabra)
    val ret = todas.groupBy(p => p.size)

    // COMO PALABRAS DE UNA SOLA LETRA, DEJAMOS SOLO a,o,y
    ret.updated(1, Array("a", "o", "y").map(Palabra(_)))
  }




  def buscaCoincidenciaMultiple(buscado: Histograma, maximo: Int = 2, previas: Seq[Palabra] = Seq()): Unit = {

    implicit class HistogramaOps(h:Histograma){

      def sumaHistograma(h2: Histograma): Histograma = {
        (for (k <- h.keys ++ h2.keys) yield {
          val v1 = h.getOrElse(k, 0)
          val v2 = h.getOrElse(k, 0)
          (k, v1 + v2)
        }).toMap
      }

      def restaHistograma(sustraendo: Histograma): Option[Histograma] = {
        val minuendo: Histograma = h
        sustraendo.entraEn(minuendo) match {
          case false =>
            None
          case true =>
            val ret = minuendo.map { case (c, n) => (c, n - sustraendo.getOrElse(c, 0)) }.
              filter { case (c, n) => n > 0 }
            Some(ret)
        }
      }

      def entraEn(contenedor: Histograma) = {
        val contenido: Histograma = h
        contenido.keys.forall(c => contenedor.isDefinedAt(c) && contenido(c) <= contenedor(c))
      }
    }

    if (maximo == 0) {
      return
    }

    val longitudIni = buscado.values.sum
    val longitudFin = previas.lastOption.map(_.palabra.size).getOrElse(1)
    for (longitud <- (longitudIni to longitudFin by -1) if palabras.contains(longitud);
      candidatas = palabras(longitud).takeWhile(p => !previas.contains(p));
      candidata <- candidatas if candidata.histograma.entraEn(buscado)) {
      lazy val siguiente = previas ++ Seq(candidata)
      if (buscado == candidata.histograma) {
        println(siguiente.map(_.palabra).mkString(" - "))
      }
      else {
        buscaCoincidenciaMultiple(buscado.restaHistograma(candidata.histograma).get, maximo - 1, siguiente)
      }
    }
  }

  def buscaCoincidenciaExacta(buscado: Palabra) = {
    for (p <- palabras(buscado.palabra.size).view if p.histograma == buscado.histograma) yield {
      p
    }
  }


  def buscaExactoEnFrase( frase: String, letras: Int ) ={

    val f = frase.split("""\s+""")

    val combinacionesDePalabrasConLetras = {
      for (from <- (0 to f.size).view;
        until <- (from to f.size).view;
        slice = f.slice(from, until) if slice.map(_.size).sum == letras) yield {
        slice.mkString
      }
    }

    for (c <- combinacionesDePalabrasConLetras;
      palabra = Palabra(c);
      p <- buscaCoincidenciaExacta(palabra)) yield {
      p
    }
  }


  def dia2018_07_14(){
    println( "SABADO 14 JULIO 2018");


    val p2 = Palabra("robarpasta"); // ocultan las partes más interesanes
    println("EXACTO:" + p2 );
    for (p <- buscaCoincidenciaExacta(p2)) {
      println(p)
    }

    val p1 = Palabra("diamanteruin"); //nada de lo que se lleva le es ajeno
    println("EXACTO:" + p1 );
    for (p <- buscaCoincidenciaExacta(p1)) {
      println(p)
    }


    val frase = "veinte lios bien liados dan para mucha audiencia"; // 10 letras
    println("EXACTO EN FRASE: " + frase)
    for (p <- buscaExactoEnFrase(frase, 10) ) {
      println(p)
    }

    val p4 = Palabra("iatstn");// si son medias no son transparentes; primera y última letra de las anteriores
    println("EXACTO:" + p4 );
    for (p <- buscaCoincidenciaExacta(p4)) {
      println(p)
    }
  }

  def dia2018_07_21(){
    println( "SABADO 21 JULIO 2018");


    val p2 = Palabra("otramadre"); // es el encargado de dar la patada definitiva
    println("EXACTO:" + p2 );
    for (p <- buscaCoincidenciaExacta(p2)) {
      println(p)
    }

    val p1 = Palabra("leercinta"); //la marcha le obliga a soplar por un tubo
    println("EXACTO:" + p1 );
    for (p <- buscaCoincidenciaExacta(p1)) {
      println(p)
    }


    val frase = "la corrupción del primo decente está fuera de lugar"; // 12 letras
    println("EXACTO EN FRASE: " + frase)
    for (p <- buscaExactoEnFrase(frase, 12) ) {
      println(p)
    }

    val p4 = Palabra("rrceie");// orden de clausura; primera y última letra de las anteriores
    println("EXACTO:" + p4 );
    for (p <- buscaCoincidenciaExacta(p4)) {
      println(p)
    }
  }

  dia2018_07_21();


  //buscaCoincidenciaMultiple(Palabra("bodacara").histograma )
}

PalabrasAnagramadas.main(args)

