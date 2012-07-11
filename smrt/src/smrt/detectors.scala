package smrt

//abstract class Detector[T] {
//  def detects(tokens: List[Token]): T
//  
//  implicit def toDetector( s: String ) = new FixedDetector( s )
//  
//  def regex( pattern: String ) = new RegexDetector( pattern )
//  def optional( d: Detector[T] ) = new OptionalDetector( d )
////  def any( dlist: Detector[_]* ) = new AnyDetector( dlist.toList )
//  
//  def separator = regex( "[\\.-:\\s]" )
//  def whitespace = regex( "\\s+" )
//  def anyToken = regex( ".+" )
//  
////  def ~( d: Detector[_] ) = new CompositeDetector( List( this, d ) )
//}
//
//class RegexDetector( pattern: String ) extends Detector[String] {
//  def detects(tokens: List[Token]) = tokens.head.toString
//}
//
//class FixedDetector( s: String ) extends Detector[String] {
//  def detects(tokens: List[Token]) = s
//}
//
//class OptionalDetector[T]( d: Detector[T] ) extends Detector[Option[T]] {
//  def detects(tokens: List[Token]) = Some( d.detects(tokens) )
//}

//class AnyDetector[T]( detectors: List[Detector[T]] ) extends Detector[T] {
//  def detects(tokens: List[Token]) = null//"TODO"
//}
//
//class CompositeDetector( detectors: List[Detector[_]] ) extends Detector {
//  override def ~( d: Detector[_] ) = new CompositeDetector( detectors :+ d )
//}
