package smrt

case class Pattern(  ) {
  def ? = new Pattern
  def ~( p2: Pattern ) = p2
}

case class TokenTest( fn: String => Boolean ) extends Pattern

object Patterns {
  def token(s: String) = ""
  def digits(n: Int) = TokenTest( s => s.size == n && s.forall(_.isDigit) )
  val whitespace = TokenTest( _.forall( _.isWhitespace ) )

  val dot  = token(".")
  val dash = token("-")
  
  def ws( tt: TokenTest ) = (whitespace?) ~ tt ~ (whitespace?)
  
//  val aDate = Pattern(
//    aDateDotted | aDateYDotted | aDate
//  )
//  val aDateDotted = Pattern(
//    aDay ~ dot ~ aMonth ~ dot
//  )
//  val aDateYDotted = Pattern(
//	aDay ~ dot ~ aMonth ~ dot ~ aYear4 ~ dot
//  )
//  val aDay = Pattern(
//    digits(4),
//    condition = { _.toInt <= 31 },
//    confidence = 150
//  )
//  val aYear4 = Pattern(
//    digits(4),
//	condition = { s => s.toInt > 2000 && s.toInt < 2050 },
//	confidence = 250
//  )
//  val dateRange = Pattern(
//    aDateDotted ~ ws(dash) ~ aDateDotted
//  )
}
