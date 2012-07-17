package smrt

import java.util._
import javax.mail._
import javax.mail.event._
import javax.mail.internet._
import javax.activation._

object ImapReader extends App {

  connect
  
  def connect() {
    val props = System.getProperties();
    props.setProperty("mail.store.protocol", "imaps");
    try {
      val session = Session.getDefaultInstance(props, null);
      val store = session.getStore("imaps");
      store.connect("imap.gmail.com", "hrvoje@apartman-tonio.com", args(0));
      val folder = store.getFolder("INBOX")
      folder.open(Folder.READ_ONLY)
      val messages = folder.getMessages
      val mails = ( messages take 39 ) flatMap { m =>
        val from = m.getFrom()(0).asInstanceOf[InternetAddress].getAddress.toLowerCase
        if (!from.endsWith("apartman-tonio.com")) {
          val mail = new Mail
          mail.from = from
          mail.subject = m.getSubject
          val c = m.getContent
          if (c.isInstanceOf[Multipart]) {
            val mp = c.asInstanceOf[Multipart]
            for ( i <- 0 to mp.getCount-1 ) {
              val bp = mp.getBodyPart(i)
              if (bp.getContentType.toLowerCase.startsWith("text/plain"))
                mail.body = bp.getContent.toString
            }
          }
          else mail.body = c.toString
          println( mail )
          Some( mail )
        }
        else None
      }
    }
    catch {
      case e => println(e)
    }
  }
}

class Mail {
  var from: String = _
  var subject: String = _
  var body: String = _
  
  override def toString = from + " [" + subject + "]\n" + body
}