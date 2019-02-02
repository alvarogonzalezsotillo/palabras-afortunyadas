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
      sinAcentos.replace(" ", "" ).replace(".","").replace(",","").replace(":","")
    }

    lazy val size = palabra.size
    lazy val palabra = quitaAcentosYEspacios(original)

    lazy val histograma : Histograma = {
      val ret = palabra.groupBy(c => c)
      ret.map { case (c, set) => (c, set.size) }
    }

    override val toString = original
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




  type Pista = (String,Any)

  def resuelvePista( pista : Pista )(implicit corpus: Corpus) = {
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
        println( s"${frase.toUpperCase}: Anagrama en la frase, longitud $size" );
        for (c <- buscaExactoEnFrase(frase, size) ) {
          println("  " + c)
        }

      case err =>
        throw new Error("Se espera String->Palabra, String->Int o String->Array[String]:" + err )
    }
  }










  val pistas : Map[String,Seq[Pista]] = Map(


    "2018-09-22" -> Seq(
      "Ha provocado la exfoliación de muchas margaritas" -> "CISNE INDIO",
      "Siempre canté muy mal, sin duda alguna" -> 12,
      "Las canarias desenfrenadas dan muestras de dulzura" -> 8,
      "Sus castillos son espectaculares, pero resultan efímeros" -> Array("Indecisión","precisamente","sacarina")
    ),

    "2018-09-29"-> Seq(
      "Es muy capaz de comerle el coco a cualquier hombre" -> "grietas",
      "No es verdad y a menudo parece mentira" -> "CLIMA RUSO",
      "Estaba hecho un andrajo pero logró completar el trabajo del día" -> 7,
      "Acortan el espacio pero pueden alargar el tiempo" -> Array("tigresa","simulacro","jornada")
    ),

    "2018-10-06" -> Seq(
      "Vino de Francia" -> "piromántico",
      "Rediseña la licorería para poder albergar buenos recuerdos" -> 9 ,
      "Vivir de administrar los remanentes de forma adecuada" -> 10,
      "Trabaja de cara a la galería" -> Array("importación","relicario","mantenerse")
    ),

    "2018-11-03" -> Seq(
      "Puede servir de ejemplo para medio mundo" -> "esa es firme",
      "Confusa plática que no siempre resulta suficientemente interesante" -> 7 ,
      "Una terrible aversión la sacó de quicio" -> 8,
      "Son pesadas, pero en el fondo son útiles" -> Array("semiesfera","capital","nerviosa","ancla")
    ),

    "2018-11-10" -> Seq(
      "Cuando le hablan del imsomnio se desvela" -> "ripio cachondo",
      "Familia muy arraigada en el campo" -> "La muesca" ,
      "La patrona se alteró, pero consiguió cerrar la boca" -> 7,
      "Pulimentos realizados sin elementos agresivos" -> Array("hipocondriaco","ulmáceas","taponar","hurtos")
    ),

    "2018-11-17" -> Seq(
      "Se doblega ante las exigencias del público, pero sólo por motivos profesionales" -> "CORISTA SIN TONO",
      "Con los románticos ridículos nos entra un sueño irresistible" -> 10,
      "Versión de teatro cheli montada como una amalgama de cosas dispersas" -> 11,
      "a la espera" -> Array("contorsionista","cartonismo","heteroclita")
    ),

    "2018-12-01" -> Seq(
      "Vence la amigdalitis retorciendo el pescuezo" -> "NOTAS ASMA",
      "Cuando la majadera se desmadra los golfos se agitan" ->8,
      "Un sinónimo intrigante que nos hace comer el coco cuando desearíamos descansar"->8,
      "Si se le mete mano se arruga" -> Array("matasanos", "marejada", "insomnio")
    ),


    "2018-12-08" -> Seq(
      "Fingen odiar cuando quieren"-> "TÍO PARCHÍS" ,
      "El arte de describir algo tan claro como el agua" -> "HORA FRÍGIDA",
      "Son terriblemente patéticos y conforman una autoridad que no está muy bien vista" -> 9,
      "Falta pista" -> Array("hipócritas", "hidrografía", "capitoste")
    ),

    "2018-12-15" -> Seq(
      "Aunque lo pongan de cara a la pared, sabe cómo devolver la pelota" -> "TROPELÍA",
      "Hablando de poetizar libremente, es una buena figura" -> 10,
      "El armatoste quedó maltrecho pero todavía está en pie" -> 9,
      "Por mucho que intentemos matarlo, nos acabará matando él" -> Array( "Pelotari", "trapezoide", "metatarso")
    ),


    "2018-12-29"-> Seq(
      "Hace que la bolsa se oiga, pero apenas la hace subir" -> "CALLA LÍDER",
      "Vacilando con las vacilonas, nos libró de una buena" -> 9 ,
      "Escribe poco y mal, y editarlo resulta raro" -> 8,
      "Representa un buen negocio para la banca" -> Array("calderilla","salvacion","iletrado","hurtos")   
    ),

    "2019-01-05" -> Seq(
      "Permite recuperar el capital invertido de una manera harto azarosa" -> "TREN REGIO",
      "Trata de imponer su ley en la banda" -> "EL ALTAR",
      "Fea, terca y hecha una furia, nos proporciona una apreciable amargura" -> 8,
      "Falta pista" -> Array("reintegro","lateral","cafetera")
    ),


    "2019-01-12" -> Seq(
      "Es bueno que tenga seriedad para las pataletas y gracia para los cuentos" -> "LUCIR PUERTO",
      "Si esto baja de una manera alarmante, la oposición será disimulada" -> 8,
      "Hipotecar sin la debida formalidad es una actividad altamente sospechosa" -> 9,
      "En las cartas suele estar escrito al final" -> Array( "puericultor", "sabotaje", "trapicheo", "postre" )
    ),

    "2019-01-19" -> Seq(
      "Es capaz de contentar al mismo tiempo al cuñado y a la suegra" -> "RATA ABISMAL",
      "Se aprecia mucho más en el Atlántico que en el Mediterráneo" -> "LAMPREA",
      "Cien ritos esotéricos de los que nadie sabe casi nada" -> 9,
      "Poderosas razones para querer dominar el partido" -> Array( "malabarista", "pleamar", "inciertos" )
    ),

    "2019-01-26" -> Seq(
      "Aumenta la talla sin aumentar el peso" -> "CLAN IDIOTA",
      "Es un acierto increíble y resulta muy sugerente" -> 7,
      "Un alcalde se trastorna y suelta una flor: qué maravilla" -> 9,
      ""-> Array("dilatación","erótica","caléndula")

    ),

    "2019-02-02" -> Seq(
      "No es raro que tenga más amigos que dinero" -> "DIETA LISA",
      "Hace un trabajo de antología" -> "ENREDO CLÁSICO"
    )

  )

  def resuelve(implicit palabras: Corpus) = {

    println( s"Corpus:${palabras.values.map(_.size).sum}" )

    for( dia <- pistas.keys.toSeq.sorted ; ps = pistas(dia) ) cronometro("Solución"){
      println( s"****** Día $dia")
      ps.foreach(resuelvePista)
    }
  }
}

// https://www.facebook.com/noesundia
