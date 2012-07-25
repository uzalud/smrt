package valids

import org.scardf._
import org.joda.time.LocalDate

object Voc2 extends Vocabulary("http://voc2.eg#") {
  val arrival = propStr("arrival")
  val departure = propStr("departure")
  val day = propStr("day")
  val month = propStr("month")
  val year = propStr("year")
}
import Voc2._

object DateTests extends App {

  for (mail <- QueryData.data) {
    test(mail.body)
  }
  def test( inString: String ) = {
    val sgs = DateRegexes.rs flatMap { _.phrase(inString) }
    println(sgs.map(_.graph.rend).mkString("\n"))
  }
    
//  test("we would like to rent an apartment in Tucepi:\n- 4 person\n- 19.07 - 29.07")
//  test("molim Vas za informaciju o cijeni (i vrsti) smještaja za dvije osobe u periodu 24.06. - 30.06.")
//  test("Dali imate slobodan smjestaj od 11-18.08 za 3 odrasle osobe mene,suprugu i brata  i dvoje manjih djece 9 i 12 godina")
//  test("Period za rezervaciju:24.06-29.06.2012 (5 noćenja)")
//  test("wir suchen Unterkunft in Termin 21.7.2012-28.7.2012.")
//  test("trazim apartman u Tucepima za 2+3 osobe (djeca 6,3,1) u periodu\n13.07.-22.07. za 7 dana, po mogucnosti u prizemlju.")
//  test("Dali imate slobodan smjestaj od 11-18.08 za 3 odrasle osobe mene,suprugu i brata i dvoje manjih djece 9 i 12 godina.")
//  test("The time range: 20.7.2012-4.8.2012 (in this time of 7 nights).\nMax. EURO 60 per apartment / night. Ideally EURO 45 - EURO 50 / night.")
//  test("Mi jesu obličje za smještaj u razdoblju 21.7.2012 - 28.7.2012. Mi smo grupa")
//  test("if you have free apartment for a\nperiod 8-14.07.2012 for 4 persons (2 adults+2 children -14 &10 years\nold).")
//  test("Dates: 4.7 - 13.7. 2012 (9 nights)")
}

object DateRegexes {
  def make( d1: Int, m1: Int, y1: Int, d2: Int, m2: Int, y2: Int ) = {
    val arr = new LocalDate(y1, m1, d1) 
    val dep = new LocalDate(y2, m2, d2)
    val focal = Blank()
    val g = Graph( focal -( arrival -> arr, departure -> dep ) )
	g/focal
  }
  
  val rs = List(
    Dexter(
      """[^0-9.](\d?\d)\.(\d?\d)\.(\d{4})\.?\s*-\s*(\d?\d)\.(\d?\d)\.(\d{4})\.?[^0-9]""", {
      case List( d1, m1, y1, d2, m2, y2 ) => 
        make(d1.toInt, m1.toInt, y1.toInt, d2.toInt, m2.toInt, y2.toInt)
    } ),
    Dexter(
      """[^0-9.](\d?\d)\.(\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.(\d{4})\.?[^0-9]""", {
      case List( d1, m1, d2, m2, y2 ) => 
        make(d1.toInt, m1.toInt, y2.toInt, d2.toInt, m2.toInt, y2.toInt)
    } ),
    Dexter(
      """[^0-9.](\d?\d)\.(\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.?[^0-9]""", {
      case List( d1, m1, d2, m2 ) =>
        val y = new LocalDate().getYear
        make(d1.toInt, m1.toInt, y, d2.toInt, m2.toInt, y)
    } ),
    Dexter(
      """[^0-9.](\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.?[^0-9]""", {
      case List( d1, d2, m2 ) =>
        val y = new LocalDate().getYear
        make(d1.toInt, m2.toInt, y, d2.toInt, m2.toInt, y)
    } )
  )
}


case class Dexter( patternString: String, extractor: PartialFunction[List[String], GraphNode] ) {
  val pattern = patternString.r
  
  def phrase( str: String ) = {
    val m = pattern.findFirstMatchIn(str)
    val result = m.map( _.subgroups ).map( extractor orElse { case _ => null } )
    if (result == Some(null)) None else result
  }
}
