package rne

import scala.scalajs.js

object Counter{
  private var _next = 0

  def next = {
    val ret = _next
    _next += 1
    ret
  }
}


object Message{
  def getJSProperty[T](o: Any)(property: String) : Option[T] = {
    val value = o.asInstanceOf[js.Dynamic].selectDynamic(property)
    if( js.isUndefined(value) ) None else Some(value.asInstanceOf[T])
  }

  def unapply( o: Any ) : Option[String] = getJSProperty[String](o)("messageType")

  object LoadCorpus{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "LoadCorpus", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("LoadCorpus") => getJSProperty[String](o)("file")
      case _ => None
    }
  }

  object CorpusLoaded{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "CorpusLoaded", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("CorpusLoaded") => getJSProperty[String](o)("file")
      case _ => None
    }
  }

  object SearchAnagram{
    def apply( anagram: String ) = js.Dynamic.literal( "messageType" -> "SearchAnagram", "anagram" -> anagram )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("SearchAnagram") => getJSProperty[String](o)("anagram")
      case _ => None
    }
  }

}



