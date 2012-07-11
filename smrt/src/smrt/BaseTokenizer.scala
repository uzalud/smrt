package smrt

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._

class SParser extends JavaTokenParsers {
  type Tokens = StdLexical
  val lexical = new StdLexical
  def digitMonth: Parser[Int] = "[01]?\\d".r ^^ {_.toInt}
  def digitDay: Parser[Int] = "[0123]?\\d".r ^^ {_.toInt}
  def digitDayMonth = digitDay ~ (( "." | "-" )?) ~ (whiteSpace?) ~ digitMonth ~ ( "."? ) ^^ {
    case d ~ _ ~ _ ~ m ~ _ => Map( 'day -> d, 'month -> m )
  }
}
