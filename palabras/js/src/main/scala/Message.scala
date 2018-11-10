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

  def getJSStr(o: Any) = getJSProperty[String](o) _

  def unapply( o: Any ) : Option[String] = getJSStr(o)("messageType")

  object LoadCorpus{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "LoadCorpus", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("LoadCorpus") => getJSStr(o)("file")
      case _ => None
    }
  }

  object CorpusLoaded{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "CorpusLoaded", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("CorpusLoaded") => getJSStr(o)("file")
      case _ => None
    }
  }

  object SearchAnagram{
    def apply( anagram: String ) = js.Dynamic.literal( "messageType" -> "SearchAnagram", "anagram" -> anagram )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("SearchAnagram") => getJSStr(o)("anagram")
      case _ => None
    }
  }

  object AnagramFound{
    def apply( found: String, anagram: String ) = js.Dynamic.literal( "messageType" -> "AnagramFound", "found" -> found, "anagram" -> anagram )
    def unapply( o: Any ) : Option[(String,String)] = o match{
      case Message("AnagramFound") =>
        for( found <- getJSStr(o)("found"); anagram <- getJSStr(o)("anagram") ) yield (found,anagram)
      case _ => None
    }
  }

  object NoMoreAnagrams{
    def apply(anagram:String) = js.Dynamic.literal( "messageType" -> "NoMoreAnagrams", "anagram" -> anagram )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("NoMoreAnagrams") => getJSStr(o)("anagram")
      case _ => None
    }
  }

  object SearchAnagramInSentence{
    def apply(anagram:String, size: Int) = js.Dynamic.literal( "messageType" -> "SearchAnagramInSentence", "anagram" -> anagram, "size" -> size )
    def unapply( o: Any ) : Option[(String,Int)] = o match{
      case Message("SearchAnagramInSentence") =>
        for( anagram <- getJSStr(o)("anagram") ; size <- getJSProperty[Int](o)("size") ) yield( anagram, size.toInt )
      case _ => None
    }
  }

}



