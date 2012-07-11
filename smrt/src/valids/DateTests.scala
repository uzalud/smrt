package valids

object DateTests extends App {

  def test( inString: String ) = {
    val sgs = DateRegexes.rs flatMap { rule =>
	  val (symbol, regex) = rule
	  val m = regex.findFirstMatchIn(inString)
	  m.map(_.subgroups)
	}
    println(sgs mkString "  ")
  }
    
  test("we would like to rent an apartment in Tucepi:\n- 4 person\n- 19.07 - 29.07")
  test("molim Vas za informaciju o cijeni (i vrsti) smještaja za dvije osobe u periodu 24.06. - 30.06.")
  test("Dali imate slobodan smjestaj od 11-18.08 za 3 odrasle osobe mene,suprugu i brata  i dvoje manjih djece 9 i 12 godina")
  test("Period za rezervaciju:24.06-29.06.2012 (5 noćenja)")
  test("wir suchen Unterkunft in Termin 21.7.2012-28.7.2012.")
  test("trazim apartman u Tucepima za 2+3 osobe (djeca 6,3,1) u periodu\n13.07.-22.07. za 7 dana, po mogucnosti u prizemlju.")
  test("Dali imate slobodan smjestaj od 11-18.08 za 3 odrasle osobe mene,suprugu i brata i dvoje manjih djece 9 i 12 godina.")
  test("The time range: 20.7.2012-4.8.2012 (in this time of 7 nights).\nMax. EURO 60 per apartment / night. Ideally EURO 45 - EURO 50 / night.")
  test("Mi jesu obličje za smještaj u razdoblju 21.7.2012 - 28.7.2012. Mi smo grupa")
  test("if you have free apartment for a\nperiod 8-14.07.2012 for 4 persons (2 adults+2 children -14 &10 years\nold).")
  test("Dates: 4.7 - 13.7. 2012 (9 nights)")
}

object DateRegexes {
  val rs = List(
    'dmydmy -> """[^0-9.](\d?\d)\.(\d?\d)\.(\d{4})\.?\s*-\s*(\d?\d)\.(\d?\d)\.(\d{4})\.?""".r,
    'dmdmy  -> """[^0-9.](\d?\d)\.(\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.(\d{4})\.?""".r,
    'dmdm   -> """[^0-9.](\d?\d)\.(\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.?""".r,
    'ddm    -> """[^0-9.](\d?\d)\.?\s*-\s*(\d?\d)\.(\d?\d)\.?""".r
  )
}