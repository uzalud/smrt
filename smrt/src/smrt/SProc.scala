package smrt

case class MultipartMessage( mps: List[MessagePart] )
case class MessagePart( partType: Zone, body: String, lang: Lang )

abstract class Zone
object GreetingZone  extends Zone
object AuthoredZone  extends Zone
object QuotedZone    extends Zone // parts from earlier messages
object SignoffZone   extends Zone // thanks, salutation, signature
object FodderZone    extends Zone // includes disclaimer, ads, antivirus signature

abstract class MessageClass
object Query extends MessageClass

object SProc extends App {

  def partition(m: Mail) = {
    MultipartMessage(Nil)
  }
  
  def classify(mm: MultipartMessage) = {
    Query
  }
  
  def process(m: Mail) = {
    val msg = partition(m)
    val mclass = classify(msg)
    mclass match {
      case Query =>
      case _ => // 
    }
  }
}