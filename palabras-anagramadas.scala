
object PalabrasAnagramadas extends App {




  import scala.collection.immutable.Map
  import scala.collection.Iterator
  import java.io._
  import java.util._


  type Frase = Array[Palabra]
  type Histograma = Map[Char, Int]

  case class Palabra(p: String) {

    import scala.concurrent.ExecutionContext.Implicits.global

    private def quitaAcentosYEspacios(s: String): String = {
      val acentos = Map(
        'á' -> 'a',
        'é' -> 'e',
        'í' -> 'i',
        'ó' -> 'o',
        'ú' -> 'u',
        'ü' -> 'u'
      )
      val sinAcentos = s.toLowerCase.map(c => if (acentos.isDefinedAt(c)) acentos(c) else c)
      sinAcentos.replace(" ", "" )
    }

    val size = palabra.size
    lazy val palabra = quitaAcentosYEspacios(p)

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

  def dia2018_09_15(){
    println( "15 septiembre 2018");

    // 1) O tiene solución o hay que llamarlo de otro modo (anagrama: MAL POBRE)
    // 2) La búsqueda de la verdad oculta (anagrama: VEIS INCÓGNITA)
    // 3) Eso de cutis tan retorcido que describe matemáticamente algunas células epiteliales (el anagrama está en la definición, la palabreja en plural tiene 10 letras y Clara Grima es una de sus responsables)
    // 4) Juega con la cabeza

    val p2 = Palabra("malpobre");
    println("EXACTO:" + p2 );
    for (p <- buscaCoincidenciaExacta(p2)) {
      println(p)
    }

    val p1 = Palabra("veisincognita"); //la marcha le obliga a soplar por un tubo
    println("EXACTO:" + p1 );
    for (p <- buscaCoincidenciaExacta(p1)) {
      println(p)
    }


    val frase = "Eso de cutis tan retorcido que describe matemáticamente algunas células epiteliales"; // 10 letras
    println("EXACTO EN FRASE: " + frase)
    for (p <- buscaExactoEnFrase(frase, 12) ) {
      println(p)
    }

    val p4 = Palabra("paesin");// problema escutoides investigacion
    println("EXACTO:" + p4 );
    for (p <- buscaCoincidenciaExacta(p4)) {
      println(p)
    }

  }

  def dia2018_09_22(){
    println( "22 septiembre 2018");

    // 1) Ha provocado la exfoliación de muchas margaritas: CISNE INDIO
    // 2) Siempre canté muy mal, sin duda alguna (en la definición, 12 letras)
    // 3) Las canarias desenfrenadas dan muestras de dulzura (en la definición, 8 letras)
    // 4) Sus castillos son espectaculares, pero resultan efímeros

    val p2 = Palabra("cisneindio");
    println("EXACTO:" + p2 );
    for (p <- buscaCoincidenciaExacta(p2)) {
      println(p)
    }

    val frase = "Siempre canté muy mal, sin duda alguna";
    println("EXACTO EN FRASE: " + frase)
    for (p <- buscaExactoEnFrase(frase, 12) ) {
      println(p)
    }

    val frase2 = "Las canarias desenfrenadas dan muestras de dulzura";
    println("EXACTO EN FRASE: " + frase2)
    for (p <- buscaExactoEnFrase(frase2, 8) ) {
      println(p)
    }


    val p4 = Palabra("inpesa");// indecisión precisamente sacarina
    println("EXACTO:" + p4 );
    for (p <- buscaCoincidenciaExacta(p4)) {
      println(p)
    }

  }

  def dia2018_09_29(){
    println( "29 septiembre 2018");

    val pistas = Array(
      "Es muy capaz de comerle el coco a cualquier hombre" -> Palabra("grietas"),
      "No es verdad y a menudo parece mentira" -> Palabra("CLIMA RUSO"),
      "Estaba hecho un andrajo pero logró completar el trabajo del día" -> 7,
      "Ahorra espacio" -> Array("tigresa","simulacro","jornada","atajos")
    );


    for( pista <- pistas ) pista match{
      // LA ULTIMA PALABRA SE CONSIGUE CON EL INICIO Y FIN DE LAS TRES PRIMERAS
      case (msg,a:Array[String]) =>
        val s = a.take(3).map( p => p.head.toString + p.last.toString ).mkString
        val p = Palabra(s);
        println(msg + " -- EXACTO:" + p );
        for (c <- buscaCoincidenciaExacta(p)) {
          println("  " + c)
        }

      // NOS DAN UNA PALABRA PARA EL ANAGRAMA
      case (msg,p:Palabra) =>
        println(msg + " -- EXACTO:" + p );
        for (c <- buscaCoincidenciaExacta(p)) {
          println("  " + c)
        }

      // EL ANAGRAMA ESTÁ EN LA DEFINICIÓN, NOS DAN EL NÚMERO DE LETRAS
      case (frase:String,size:Int) =>
        println("EXACTO EN FRASE: " + frase)
        for (c <- buscaExactoEnFrase(frase, size) ) {
          println("  " + c)
        }

      case _ =>
        throw new Error("Se espera String->Palabra, String->Int o String->Array[String]" )
        
        
    }
  }

  dia2018_09_29();
}


