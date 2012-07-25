package smrt

import scala.math._

abstract class MembFn extends Function[Double, Double] {
  final def apply( x: Double ): Double = max( 0.0d, min( fn(x), 1.0d ) )
  
  def fn( x: Double ): Double
  
  def unary_- = new MembFn {
    val f1 = this
    def fn( x: Double ) = 1 - f1(x)
  }
  
  def |( f2: MembFn ) = {
    val f1 = this
    new MembFn { def fn( x: Double ) = max( f1(x), f2(x) ) }
  }
  
  def &( f2: MembFn ) = {
	val f1 = this
	new MembFn { def fn( x: Double ) = min( f1(x), f2(x) ) }
  }
  
  def +( f2: MembFn ) = {
    val f1 = this
    new MembFn {
      def fn( x: Double ) = {
        val y1 = f1(x)
        val y2 = f2(x)
        y1 + y2 - y1*y2
      }
    }
  }
}

class TrapezeMembFn( min: Double, midLow: Double, midHigh: Double, max: Double ) extends MembFn {
  def fn( x: Double ): Double = {
    if( x < min || x > max ) return 0
    if( x >= midLow && x <= midHigh ) return 1
	if( x < midLow ) return (x - min)/(midLow - min)
	1 - (x - midHigh)/(max - midHigh)
  }
}

class SigmoidalMembFn( gain: Double, t0: Double ) extends MembFn {
  def fn( x: Double ) = 1d / (1d + exp(-gain * (x - t0)))
}

class GaussMembFn( mean: Double, stdev: Double ) extends MembFn {
  def fn( x: Double ) = exp( -(x - mean)*(x - mean) / (2*stdev*stdev) )
}

class CExpMembFn( lambda: Double ) extends MembFn {
  def fn( x: Double ) = 1 - exp( -lambda*x )
}

import scala.Double.{ PositiveInfinity, NegativeInfinity }
object MembFn {
  def ramp( range: Pair[Double, Double] ) = new TrapezeMembFn( range._1, range._2, PositiveInfinity, PositiveInfinity )
  def slide( range: Pair[Double, Double] ) = new TrapezeMembFn( NegativeInfinity, NegativeInfinity, range._1, range._2 )
  def triangle( min: Double, mid: Double, max: Double ) = new TrapezeMembFn( min, mid, mid, max )
  def trapeze( min: Double, midLow: Double, midHigh: Double, max: Double ) = new TrapezeMembFn( min, midLow, midHigh, max )
  def sigma( gain: Double, inflexion: Double ) = new SigmoidalMembFn( gain, inflexion )
  def gauss( mean: Double, stdev: Double ) = new GaussMembFn( mean, stdev )
  def exp( lambda: Double ) = new CExpMembFn( lambda )
}
