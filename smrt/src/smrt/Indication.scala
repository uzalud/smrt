package smrt

import scala.collection.immutable.List

case class Fragment( full: List[String], start: Int, end: Int ) {
  
  private def decute(ch: Char) = ch match {
    case 'š' => 's'
    case 'đ' => 'd'
    case 'č' => 'c'
    case 'ć' => 'c'
    case 'ž' => 'z'
    case _ => ch
  }
  
  lazy val lines = full.slice(start, end+1)
  lazy val length = end-start+1
  lazy val words = lines.mkString(" ").toLowerCase.map(decute).split("\\W").map(_.trim).filterNot(_.isEmpty).toSet
  
  lazy val prevLine = if (start > 0) full(start-1) else ""
    
  lazy val prevNWLine: Option[String] =
    if ( start == 0 ) None
    else if ( prevLine.matches("\\s*") ) Fragment(full, start-1, end).prevNWLine
    else Some(prevLine)
    
  lazy val firstLine = line(0)
  def line(n: Int): String = if (start+n >= 0 && start+n <= end) full( start+n ) else ""
}

case class ZoneIndication( symptom: Symptom, relevancy: Int => Double, diagnosis: Zone )

case class Symptom( id: Symbol, fn: Fragment => Int ) {
  def apply(f: Fragment) = fn(f)
}

object Symptom {
  def pSymptom( id: Symbol, b: Fragment => Boolean ) = apply( id, f => if (b(f)) 100 else 0 )
  
  val greetings = Set("pozdrav", "postovani", "postovana", "bok", "zdravo", "greetings", "hi", "hello")
  val signoffs = Set("hvala", "pozdrav", "postovanje", "srdacan", "srdacne", "regards", "cheers", "thank", "thanks")
  
  val wordCount = Symptom( 'wc, _.lines.map(_.split("\\s").length).sum )
  val charCount = Symptom( 'chc, _.lines.map(_.length).sum )
  val firstLinePosition = Symptom( 'flpos, f => f.start*100/f.full.length )
  val lastLinePosition = Symptom( 'llpos, f => f.end*100/f.full.length )
  //val averageLineCharCount = contains
  
  val quoteCharStarting = Symptom( 'qpfx, f => f.lines.filter(_.startsWith(">")).length*100 / f.length )  // perc of lines starting with the character ‘>’
  val precededByDelimiter = pSymptom( 'pdel, _.prevNWLine.map(_.matches("[-_=]{4}.*")) == Some(true) )
  
  val greetingWords = pSymptom( 'gw, !_.words.intersect(greetings).isEmpty )
  val signoffWords = pSymptom( 'sow, !_.words.intersect(signoffs).isEmpty )
  
  // whether a prior text fragment in the message contains a quoted header;
  // whether the text fragment contains a URL;
  // whether the text fragment contains an email address;
  // whether the text fragment contains a sequence of four or more digits;
  // the number of capitalised words in the text fragment;
  // the percentage of capitalised words in the text fragment;
  // the number of non-alpha-numeric characters in the text fragment;
  // the percentage of non-alpha-numeric characters in the text fragment;
  // the number of numeric characters in the text fragment;
  // the percentage of numeric characters in the text fragment;
  // whether the message subject line contains a reply syntax marker such as Re: ; and

  val ALL = List( wordCount, firstLinePosition, quoteCharStarting, precededByDelimiter, greetingWords, signoffWords )
}

// the length of the text fragment (in characters) relative to the previous fragment;
//• the length of the text fragment (in characters) relative to the following fragment;
//• the number of blank lines preceding the text fragment
//• the number of blank lines following the text fragment.

object TestSymptoms extends App {
  val sampleText = """Poštovani,

Nešto sam vas htio pitati, evo ovako to ide.

Lijep pozdrav,
Moje Ime
_______________________
Bla bla disklejmer bla bla
"""
  val lines = sampleText.split("\n").map(_.trim).toList
  def f( start: Int, end: Int ) = Fragment(lines, start, end)
  val fragments = List( f(0,0), f(2,2), f(4,5), f(7,7) )
  
  for ( fr <- fragments ) {
    val ha = Symptom.ALL.map( s => s.id -> s(fr) )
    println( fr.firstLine + "\n" + ha.map( p=> p._1.name + "=" + p._2 ).mkString(" ") )
  }
}