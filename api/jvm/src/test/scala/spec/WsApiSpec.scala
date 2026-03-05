package spec

import fabric.rw.*
import org.scalatest.concurrent.Eventually.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.api.WsClient
import spice.api.server.WsServer
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.net.*

import scala.collection.mutable.ListBuffer

// Shared event types
case class UserEvent(id: String, name: String)
object UserEvent {
  given rw: RW[UserEvent] = RW.gen
}

// Shared event trait
trait UserEvents {
  def userCreated(user: UserEvent): Task[Unit]
  def notification(message: String, level: String): Task[Unit]
}

object WsApiSpec {
  val receivedUsers: ListBuffer[UserEvent] = ListBuffer.empty
  val receivedNotifications: ListBuffer[(String, String)] = ListBuffer.empty

  class MyEventHandler extends UserEvents {
    override def userCreated(user: UserEvent): Task[Unit] = Task {
      receivedUsers.synchronized { receivedUsers += user }
    }
    override def notification(message: String, level: String): Task[Unit] = Task {
      receivedNotifications.synchronized { receivedNotifications += ((message, level)) }
    }
  }
}

class WsApiSpec extends AnyWordSpec with Matchers {
  import WsApiSpec.*

  "WsApiSpec" should {
    val server = new MutableHttpServer {}
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

    "configure and start server with WS" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      val events = WsServer.derive[UserEvents](server, path"/ws/events")
      server.start().sync()
      server.isRunning should be(true)

      // Connect client
      val ws = WsClient.connect[UserEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events"),
        new MyEventHandler
      ).sync()
      Thread.sleep(500)
      ws.status() should be(spice.http.ConnectionStatus.Open)

      // Broadcast from server
      events.userCreated(UserEvent("1", "Alice")).sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        receivedUsers.toList should be(List(UserEvent("1", "Alice")))
      }

      events.notification("Hello", "info").sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        receivedNotifications.toList should be(List(("Hello", "info")))
      }

      server.stop().sync()
    }
  }
}
