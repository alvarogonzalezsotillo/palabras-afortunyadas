package rne




object Corpus{

  type Corpus = Map[Int, Array[Corpus.Palabra]]
  type Histograma = Map[Char, Int]

  import Cronometro._

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

    lazy val fin = palabra.slice(palabra.size-2,palabra.size)
    lazy val inicio = palabra.slice(0,2)

    override val toString = original
  }


  val regex = """(?:\s*)(?:(\d|\.)*)(?:\s*)(\S*).*""".r

  def palabras(array: Array[Array[String]]): Corpus = cronometro("Lectura de palabras de array"){
    array.zipWithIndex.
      map{
        case (a:Array[String],i:Int ) =>
          val palabras = a.map( s => Palabra(s) )
          (i+1,palabras)
      }.
      toMap
  }


  def palabras(lineIterator: Iterator[String]): Corpus = cronometro("Lectura de palabras de fichero original"){

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

object Cronometro{
  def cronometro[T](msg: String)( proc : => T ) = {
    val ini = System.currentTimeMillis()
    val ret = proc
    val fin = System.currentTimeMillis()
    println( s"$msg: ${fin-ini} ms" )
    ret
  }
}

import Corpus.Corpus

class Experimiento(longitudes: Array[Array[Int]], circular: Boolean = true )(implicit palabras: Corpus){

  var iteradores : Array[BufferedIterator[Array[Corpus.Palabra]]] = new Array(longitudes.size)

  def fin(s:String ) = s.slice(s.size-2,s.size)
  def inicio( s:String ) = s.slice(0,2)

  def inicializaIterador(indice:Int) : Unit = {
    val i =  if(indice == 0){
      None
    }
    else{
      val previo = getIterador(indice-1).head.last.fin
      Some(previo)
    }

    val f = if(indice == longitudes.size -1 && circular){
      val siguiente = getIterador(0).head.head.inicio
      Some(siguiente)
    }
    else{
      None
    }

    iteradores(indice) = PalabrasEncadenadas.lista(longitudes(indice),i,f).buffered
  }

  def getIterador(i: Int) = {
    if( iteradores(i) == null ){
      inicializaIterador(i)
    }
    iteradores(i)
  }

  def siguiente(indice:Int) : Array[Corpus.Palabra] = {
    val it = getIterador(indice)
    if( !it.hasNext ){
      if( indice == 0 ){
        return null
      }
      if( siguiente(indice-1) == null ){
        return null;
      }
    }
    for( i <- indice+1 until longitudes.size ){
      iteradores(i) = null
    }
    iteradores(indice).next
  }

  def siguiente() : Array[Array[Corpus.Palabra]] = {
    if( siguiente(longitudes.size-1) == null ){
      return null
    }
    iteradores.map( _.head )
  }

}

object PalabrasEncadenadas {

  import Corpus.Corpus
  import Corpus.Palabra
  import scala.reflect.ClassTag


  def productoCartesiano[T:ClassTag]( a: Iterator[Array[T]], b: Iterable[T], condicion: (Array[T],T)=>Boolean = (_:Array[T],_:T) => true ) : Iterator[Array[T]] = {
    val ret = for( aa <- a ; bb <- b.iterator if condicion(aa,bb) ) yield aa:+bb
    ret
  }

  def productosCartesianos[T:ClassTag]( iterables: Seq[Iterable[T]])(condicion : (Array[T],T) => Boolean = (_:Array[T],_:T) => true  )  : Iterator[Array[T]]= {
    val primero = iterables(0).
      filter( e => condicion(Array(),e) ).
      map( Array(_) )

    iterables.drop(1).foldLeft(primero.iterator){ (producto,iterable) =>
      productoCartesiano(producto,iterable,condicion)
    }
  }

  def lista( longitudes: Array[Int], inicio: Option[String] = None, fin: Option[String] = None )( implicit palabras: Corpus) : Iterator[Array[Palabra]]= {
    assert( inicio.map( _.size <= longitudes.head).getOrElse(true) )
    assert( fin.map( _.size <= longitudes.last).getOrElse(true) )

    val candidatos = longitudes.map( palabras(_).toIterable )

    productosCartesianos(candidatos){ (previos,actual) =>
      val aceptoInicio = previos.size > 0 || inicio.map( _ == actual.inicio ).getOrElse(true)
      val aceptoFin = previos.size < longitudes.size-1 || fin.map( _ == actual.fin ).getOrElse(true)
      val ret = aceptoInicio && aceptoFin
      //if( ret ) println( s"previos:${previos.mkString(",")} actual:$actual aceptoInicio:$aceptoInicio aceptoFin:$aceptoFin inicio:$inicio fin:$fin")
      ret
    }

  }


  def busca( longitudes: Array[Array[Int]] )(implicit palabras: Corpus) : Iterator[ Array[String] ] = {


    assert( longitudes.size == 4 )


    def junta[T]( a: Array[T] ) = a.mkString(" ")

    for( p1 <- lista(longitudes(0)) ;
      //_ = println( s"p1:${p1.mkString(" ")}") ;
      p2 <- lista(longitudes(1), Some( p1.last.fin ) ) ;
      //_ = println( s"p1:${p1.mkString(" ")} p2:${p2.mkString(" ")}");
      p3 <- lista(longitudes(2), Some( p2.last.fin ) ) ;
      //_ = println( s"p1:${p1.mkString(" ")} p2:${p2.mkString(" ")} p3:${p3.mkString(" ")} ");
      p4 <- lista(longitudes(3), Some( p3.last.fin ), Some( p1.head.inicio ) ) ) yield{
      Array( junta(p1), junta(p2), junta(p3), junta(p4) )
    }


  }

}

object PalabrasAnagramadas {


  import scala.collection.immutable.Map
  import scala.collection.Iterator
  import Corpus.Corpus
  import Corpus.Palabra
  import Corpus.Histograma
  import Cronometro.cronometro


  type Frase = Array[Palabra]




  def buscaCoincidenciaMultiple(buscado: Histograma, maximo: Int = 40, previas: Seq[Palabra] = Seq())(implicit palabras: Corpus): Unit = {

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
      "Hace un trabajo de antología" -> "ENREDO CLÁSICO",
      "Para hacer teatro me basta con ponerme frac en plan disfraz" -> 11,
      "Algunas de sus persianas son utilizadas para fabricar alfombras" -> Array("idealista","seleccionador","performance")
    ),

    "2019-02-09" -> Seq(
      "Engancharse con el pico" -> "TRETA MADURA",
      "Mil africanos bien conjuntados pueden hacer muchas orquestas" ->12,
      "Di una mentira inconcebible, pero que tenga mucha tela" -> 12,
      "" -> Array("tartamudear", "filarmónicas", "indumentaria")
    ),

    "2019-03-23" -> Seq(
      "cuando la mente y la razón se alían surge la idea" ->11
    ),


    "2019-03-30" -> Seq(
      "Ni mil policías pueden detenerlo" ->  "MAPA LIBRE",
      "Por lo que respecta a los cantos, es una buena regla de tres" -> "BAR CANTO",
      "El placer es perverso y caprichoso, pero es bueno conocerlo" -> 6,
      "" -> Array("percal","imparable","cartabon")
    ),

    "2019-04-06" -> Seq(

      "Marca que se mantiene firme con el paso del tiempo" ->  "BROMA LIBRE",
      "A la hora de mirar las partes nos obliga a dejar la pasión aparte" -> "CARIDAD LIMPIA",
      "Carmen ama con locura al inglés de las películas" -> 9,
      "Es fácil verlo con el corazón en la mano" -> Array("imborrable", "imparcialidad", "cameraman", "indice")
    ),

    "2019-04-13" -> Seq(
      "Sus predicciones suelen ser más fiables que las de cualquier adivino" -> "VINDICAR EL TÉ",
      "No se quita los cascos ni para dormir" -> "ALGÚN DÚO",
      "Un cretino loco seguido de cien elementos peligrosos" -> 9,
      "" -> Array("clarividente","ungulado","centurión","cuenco")
    ),

    "2019-04-27" -> Seq(
      "Hacer los cantos menos agudos" ->"SILBARÉ",
      "Surge sin prejuicios sobre la marcha" -> "NOVICIO SIMPAR",
      "Es un lío zafio e increible, parece una auténtica animalada" -> 8,
      "Permite dar la mano usando la muñeca." -> Array("biselar","improvisación", "zoofilia", "barniz")
    ),

    "2019-05-04" -> Seq(
      "Sus creaciones tienen un alto contenido espiritual" -> "SOLICITAR",
      "Era un agente libre, pero se las arregló para poder participar en la lucha" -> 11,
      "Abonar a voleo permite llegar a la raíz" -> 6,
      "Si no nos deja jugar, miente" -> Array( "licorista","beligerante","rábano", "bolera")

    ),

    "2019-05-11" -> Seq(
      "No son fáciles de ver, pero forman parte de nuestro sistema" -> "AIRES DE TOS",
      "Con mi arte traicionero podré dar gato por liebre" -> 6,
      " Están todos muy alterados porque se están poniendo negros"-> 10,
      "" -> Array("asteroides","timaré","tostándose","esteta")
    ),

    "2019-05-18" -> Seq(
      "si actua con la mano en el corazon no triunfara" -> "Bar exodo",
      "" -> "tonel caro",
      "" -> "asilo unido",
      "" -> Array("boxeador", "colorante", "ilusionado")
    )
  )

  def resuelveEncadenadas(implicit palabras: Corpus) = {

    cronometro("Productos cartesianos"){
      val pc = PalabrasEncadenadas.productosCartesianos(
        Seq(
          Array("uno", "dos", "tres" ),
          Array("one", "two", "three"),
          Array("I", "II", "III")
        ).map( _.toIterable )
      )()

      for( x <- pc ) println( x.mkString(" -- ") )
    }



    cronometro( "Lista "){
      val l = PalabrasEncadenadas.lista( Array(4,4), Some("te"), Some("do"))
      for( x <- l.drop(100).take(100) ) println( x.mkString(" -- ") )
    }

    cronometro("palabras encadenadas"){
      implicit def intToArray(i:Int) : Array[Int] = Array() :+ i

      // 1) Es partidario de atacar el problema de raíz. (7 letras)
      // 2) Llenar de líquido sin satisfacer el amor propio. (6 letras)
      // 3) Lo que llevan todos los cominos. (5 letras)
      // 4) Ahoga el mar y deja la tierra seca. (dos palabras de 5 letras cada una)

      cronometro("Primeras palabras"){

        val e = new Experimiento(Array(7,6,5,Array(5,5)))

        for( _ <- 0 to 2000 ){
          val s = e.siguiente
          if( s == null ){
            println( "Ya no quedan más")
          }
          else{
            println( s.map(_.mkString(" ")).mkString( " -- ") )
          }
          e.siguiente(0)
        }

      }


    }
  }

  def resuelve(implicit palabras: Corpus) = {

    //resuelveEncadenadas
    //System.exit(0)

    val dia = pistas.keys.toSeq.sorted.last
    val  ps = pistas(dia)
    cronometro("Solución"){
      println( s"****** Día $dia")
      ps.foreach(resuelvePista)
    }
  }
  
  def resuelveTodas(implicit palabras: Corpus) = {

    println( s"Corpus:${palabras.values.map(_.size).sum}" )

    cronometro("Todas las soluciones"){
      for( dia <- pistas.keys.toSeq.sorted ; ps = pistas(dia) ) cronometro(s"Solución del día $dia"){
        println( s"****** Día $dia")
        ps.foreach(resuelvePista)
      }
    }
  }
}

// https://www.facebook.com/noesundia
