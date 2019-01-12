package rne




object Corpus{

  type Corpus = Map[Int, Array[Corpus.Palabra]]
  type Histograma = Map[Char, Int]


  case class Palabra(original: String) {


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

    lazy val size = palabra.size
    lazy val palabra = quitaAcentosYEspacios(original)

    lazy val histograma : Histograma = {
      val ret = palabra.groupBy(c => c)
      ret.map { case (c, set) => (c, set.size) }
    }
  }


  val regex = """(?:\s*)(?:(\d|\.)*)(?:\s*)(\S*).*""".r

  def palabras(array: Array[Array[String]]): Corpus = PalabrasAnagramadas.cronometro("Lectura de palabras de array"){
    array.zipWithIndex.
      map{
        case (a:Array[String],i:Int ) =>
          val palabras = a.map( s => Palabra(s) )
          (i+1,palabras)
      }.
      toMap
  }


  def palabras(lineIterator: Iterator[String]): Corpus = PalabrasAnagramadas.cronometro("Lectura de palabras de fichero original"){

    val iterator = lineIterator.map{ line =>
      regex.findAllMatchIn(line).next.subgroups(1)
    }

    val limite = 3000000
    val todas = iterator.take(limite).filter(_!="").map(p => Palabra(p)).toArray
    println( s"todas:${todas.size}")
    val ret = todas.groupBy(p => p.size)

    // COMO PALABRAS DE UNA SOLA LETRA, DEJAMOS SOLO a,o,y
    ret.updated(1, Array("a", "o", "y").map(Palabra(_)))
  }
}

object PalabrasAnagramadas {

  import scala.collection.immutable.Map
  import scala.collection.Iterator
  import Corpus.Corpus
  import Corpus.Palabra
  import Corpus.Histograma


  type Frase = Array[Palabra]


  def cronometro[T](msg: String)( proc : => T ) = {
    val ini = System.currentTimeMillis()
    val ret = proc
    val fin = System.currentTimeMillis()
    println( s"$msg: ${fin-ini} ms" )
    ret
  }


  def buscaCoincidenciaMultiple(buscado: Histograma, maximo: Int = 2, previas: Seq[Palabra] = Seq())(implicit palabras: Corpus): Unit = {

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

  def buscaCoincidenciaExacta(buscado: Palabra)(implicit palabras: Corpus) = {
    palabras.get(buscado.palabra.size) match{
      case Some(candidatas) => candidatas.view.filter( _.histograma == buscado.histograma )
      case None => Seq()
    }
  }


  def buscaExactoEnFrase( frase: String, letras: Int )(implicit palabras: Corpus) ={

    val toRemove = Seq( ".", ",", ":", ";", "-", "/", "'", "\"")
    val f = toRemove.
      foldLeft(frase)( (f,remove) => f.replace(remove," ")).
      split("""\s+""")

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


  def dia2018_07_14()(implicit palabras: Corpus){
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



  def dia2018_07_21()(implicit palabras: Corpus){
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

  def dia2018_09_15()(implicit palabras: Corpus){
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


  def resuelvePista( pista : (String,Any) )(implicit corpus: Corpus) = {
    pista match{
      // LA ULTIMA PALABRA SE CONSIGUE CON EL INICIO Y FIN DE LAS TRES PRIMERAS
      case (msg, a:Array[String]) =>
        val palabras = a.take(3)
        println( s"${msg.toUpperCase}: Con inicio y fin de ${palabras.mkString(",")}" );
        val s = palabras.map( p => p.head.toString + p.last.toString ).mkString
        val p = Palabra(s);
        for (c <- buscaCoincidenciaExacta(p)) {
          println("  " + c)
        }

      // NOS DAN UNA PALABRA PARA EL ANAGRAMA
      case (msg,p:Palabra) =>
        println( s"${msg.toUpperCase}: Con anagrama $p" );
        for (c <- buscaCoincidenciaExacta(p)) {
          println("  " + c)
        }

      // NOS DAN UNA PALABRA PARA EL ANAGRAMA
      case (msg,p:String) =>
        println( s"${msg.toUpperCase}: Con anagrama $p" );
        for (c <- buscaCoincidenciaExacta(Palabra(p)) ) {
          println("  " + c)
        }
       
      // EL ANAGRAMA ESTÁ EN LA DEFINICIÓN, NOS DAN EL NÚMERO DE LETRAS
      case (frase,size:Int) =>
        println( s"${frase.toUpperCase}: Anagrama en la fase, longitud $size" );
        for (c <- buscaExactoEnFrase(frase, size) ) {
          println("  " + c)
        }

      case _ =>
        throw new Error("Se espera String->Palabra, String->Int o String->Array[String]" )
    }
  }


  def dia2018_09_22()(implicit palabras: Corpus){
    println( "*********** 22 septiembre 2018");

    val pistas = Seq(
      "Ha provocado la exfoliación de muchas margaritas" -> Palabra( "CISNE INDIO" ),
      "Siempre canté muy mal, sin duda alguna" -> 12,
      "Las canarias desenfrenadas dan muestras de dulzura" -> 8,
      "Sus castillos son espectaculares, pero resultan efímeros" -> Array("Indecisión","precisamente","sacarina")
    )

    pistas.foreach(resuelvePista)
  }


  def dia2018_09_29()(implicit palabras: Corpus){
    println( "*********** 29 septiembre 2018");

    val pistas = Seq(
      "Es muy capaz de comerle el coco a cualquier hombre" -> Palabra("grietas"),
      "No es verdad y a menudo parece mentira" -> Palabra("CLIMA RUSO"),
      "Estaba hecho un andrajo pero logró completar el trabajo del día" -> 7,
      "Acortan el espacio pero pueden alargar el tiempo" -> Array("tigresa","simulacro","jornada")
    );


    pistas.foreach( resuelvePista );
  }

  def dia2018_10_06()(implicit palabras: Corpus){
    println( "************ 6 octubre 2018");

    val pistas = Seq(
      "Vino de Francia" -> Palabra("piromántico"),
      "Rediseña la licorería para poder albergar buenos recuerdos" -> 9 ,
      "Vivir de administrar los remanentes de forma adecuada" -> 10,
      "Trabaja de cara a la galería" -> Array("importación","relicario","mantenerse")
    );

    pistas.foreach( resuelvePista );
  }

  def dia2018_11_03()(implicit palabras: Corpus){
    println( "************ 3 noviembre 2018");

    val pistas = Seq(
      "Puede servir de ejemplo para medio mundo" -> Palabra("esa es firme"),
      "Confusa plática que no siempre resulta suficientemente interesante" -> 7 ,
      "Una terrible aversión la sacó de quicio" -> 8,
      "Son pesadas, pero en el fondo son útiles" -> Array("semiesfera","capital","nerviosa","ancla")
    );

    pistas.foreach( resuelvePista );
  }

  def dia2018_11_10()(implicit palabras: Corpus){
    println( "************ 10 noviembre 2018");

    val pistas = Seq(
      "Cuando le hablan del imsomnio se desvela" -> Palabra("ripio cachondo"),
      "Familia muy arraigada en el campo" -> Palabra("La muesca") ,
      "La patrona se alteró, pero consiguió cerrar la boca" -> 7,
      "Pulimentos realizados sin elementos agresivos" -> Array("hipocondriaco","ulmáceas","taponar","hurtos")
    );

    pistas.foreach( resuelvePista );
  }


  def dia2018_11_17()(implicit palabras: Corpus){

    println( "*********** 17 noviembre 2018")

    val pistas = Seq(
      "Se doblega ante las exigencias del público, pero sólo por motivos profesionales" -> Palabra("CORISTA SIN TONO"),
      "Con los románticos ridículos nos entra un sueño irresistible" -> 10,
      "Versión de teatro cheli montada como una amalgama de cosas dispersas" -> 11,
      "a la espera" -> Array("contorsionista","cartonismo","heteroclita")
    )

    pistas.foreach( resuelvePista )
  }

  def dia2018_12_01()(implicit palabras: Corpus){

    println( "*********** 1 diciembre 2018")

    val pistas = Seq(
      "Vence la amigdalitis retorciendo el pescuezo" -> Palabra("NOTAS ASMA"),
      "Cuando la majadera se desmadra los golfos se agitan" ->8,
      "Un sinónimo intrigante que nos hace comer el coco cuando desearíamos descansar"->8,
      "Si se le mete mano se arruga" -> Array("matasanos", "marejada", "insomnio")
    )

    pistas.foreach( resuelvePista )
  }

  def dia2018_12_08()(implicit palabras: Corpus){

    println( "*********** 8 diciembre 2018")

    val pistas = Seq(
      "Fingen odiar cuando quieren"-> Palabra( "TÍO PARCHÍS" ),
      "El arte de describir algo tan claro como el agua" -> Palabra("HORA FRÍGIDA"),
      "Son terriblemente patéticos y conforman una autoridad que no está muy bien vista" -> 9,
      "Falta pista" -> Array("hipócritas", "hidrografía", "capitoste")
    )
    pistas.foreach( resuelvePista )
  }

  def dia2018_12_15()(implicit palabras: Corpus){

    println( "*********** 15 diciembre 2018")

    val pistas = Seq(

      "Aunque lo pongan de cara a la pared, sabe cómo devolver la pelota" ->  Palabra("TROPELÍA"),
      "Hablando de poetizar libremente, es una buena figura" -> 10,
      "El armatoste quedó maltrecho pero todavía está en pie" -> 9,
      "Por mucho que intentemos matarlo, nos acabará matando él" -> Array( "Pelotari", "trapezoide", "metatarso")
    )

    pistas.foreach( resuelvePista )
  }

  def dia2019_01_12()(implicit palabras: Corpus){

    println( "*********** 12 enero 2019")

    val pistas = Seq(
      "Es bueno que tenga seriedad para las pataletas y gracia para los cuentos" -> "LUCIR PUERTO",
      "Si esto baja de una manera alarmante, la oposición será disimulada" -> 8,
      "Hipotecar sin la debida formalidad es una actividad altamente sospechosa" -> 9,
      "En las cartas suele estar escrito al final" -> Array( "puericultor", "sabotaje", "trapicheo" )
    )

    pistas.foreach( resuelvePista )
  }


  def resuelve(implicit palabras: Corpus) = {

    println( s"Corpus:${palabras.values.map(_.size).sum}" )

    cronometro("Solución"){
      dia2019_01_12()
    }
  }
}

// https://www.facebook.com/noesundia
