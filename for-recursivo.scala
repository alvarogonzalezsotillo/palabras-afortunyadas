




def arbol( posibilidades: Seq[Char], nivel: Int, acumulado: Seq[Char] = Seq() ) : Seq[Seq[Char]] = {

  nivel match{
    case 0 =>
      Seq(acumulado)
    case _ =>
      val hijos =
        for( c <- posibilidades.view ;
          seq <- arbol(posibilidades,nivel-1,acumulado ++ Seq(c) )
        ) yield{
          seq
      }

      Seq(acumulado).view ++ hijos
  }
}

val unArbol = arbol("1234",3)
println( "ARBOL HECHO")

for( a <- unArbol ){
  println( a )
}
