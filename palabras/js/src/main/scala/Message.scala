package rne

import scala.scalajs.js

object Message{
  def jsProp[T](o: Any)(property: String) : Option[T] = {
    val value = o.asInstanceOf[js.Dynamic].selectDynamic(property)
    if( js.isUndefined(value) ) None else Some(value.asInstanceOf[T])
  }

  def jsStr(o: Any) = jsProp[String](o) _

  def unapply( o: Any ) : Option[String] = jsStr(o)("messageType")

  object LoadCorpus{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "LoadCorpus", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("LoadCorpus") => jsStr(o)("file")
      case _ => None
    }
  }

  object CorpusLoaded{
    def apply( file: String ) = js.Dynamic.literal( "messageType" -> "CorpusLoaded", "file" -> file )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("CorpusLoaded") => jsStr(o)("file")
      case _ => None
    }
  }

  object SearchAnagram{
    def apply( anagram: String ) = js.Dynamic.literal( "messageType" -> "SearchAnagram", "anagram" -> anagram )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("SearchAnagram") => jsStr(o)("anagram")
      case _ => None
    }
  }

  object AnagramFound{
    def apply( found: String, anagram: String ) = js.Dynamic.literal( "messageType" -> "AnagramFound", "found" -> found, "anagram" -> anagram )
    def unapply( o: Any ) : Option[(String,String)] = o match{
      case Message("AnagramFound") =>
        for( found <- jsStr(o)("found"); anagram <- jsStr(o)("anagram") ) yield (found,anagram)
      case _ => None
    }
  }

  object NoMoreAnagrams{
    def apply(anagram:String) = js.Dynamic.literal( "messageType" -> "NoMoreAnagrams", "anagram" -> anagram )
    def unapply( o: Any ) : Option[String] = o match{
      case Message("NoMoreAnagrams") => jsStr(o)("anagram")
      case _ => None
    }
  }

  object SearchAnagramInSentence{
    def apply(anagram:String, size: Int) = js.Dynamic.literal( "messageType" -> "SearchAnagramInSentence", "anagram" -> anagram, "size" -> size )
    def unapply( o: Any ) : Option[(String,Int)] = o match{
      case Message("SearchAnagramInSentence") =>
        for( anagram <- jsStr(o)("anagram") ; size <- jsProp[Int](o)("size") ) yield( anagram, size.toInt )
      case _ => None
    }
  }

}



