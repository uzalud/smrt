package smrt

import org.scardf._
import NodeConverter._

object Voc extends Vocabulary("http://voc.eg#") {
  val qname = propStr("qname")
}
import Voc._

object Writr extends App {
  val queryNode = Blank("qn")
  val g = Graph(
    queryNode - qname -> "Ivan"
  )
  val s = g/queryNode
  val msg = new Msg(List(new Greeting, new Salutation))
  println(msg.compose(EN, s))
}

class Msg( parts: List[MsgPart] ) {
  def compose( lang: Lang, epi: GraphNode ) = 
    parts.map( _.compose(lang, epi) ).mkString 
}
abstract class MsgPart {
  def compose( lang: Lang, epi: GraphNode ): String
}

case class Greeting extends MsgPart {
  def compose( lang: Lang, epi: GraphNode ) = {
    val rname = epi/qname/asString.option
    lang match {
      case HR => "Poštovani,\n\n"
      case EN => "Dear Mr/Mrs. " + rname + ",\n\n"
    }
  }
}

case class Salutation extends MsgPart {
  def compose( lang: Lang, epi: GraphNode ) = {
	val rname = epi/qname/asString
    lang match {
      case HR => "Lijep pozdrav,\n\nObitelj Šimić"
      case EN => "Best regards,\n\nFamily Simic"
    }
  }
}

case class Lang( code: String )
case object HR extends Lang("hr")
case object EN extends Lang("en")
