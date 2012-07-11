package smrt

object Hullo extends App {
  val bt = new SParser
  val result = bt.parse(bt.digitDayMonth, """1.12.""")
  println(result)
//  val sp = new SParser
//  val r2 = sp.parseAll(sp.digitDayMonth, "10.32.")
//  println(r2)
//  println((new SmrtDetector).digitDayMonth)
}

//class SmrtDetector extends Detector {
//  
//  def digitMonth = regex( "[01]?\\d" ) -> {_.toInt}
//  def digitDay = regex( "[0123]?\\d" )
//
//  def digitDayMonth = digitDay ~ any( ".", "-", whitespace ) ~ digitMonth ~ optional( "." )
//}

