package spice.net

import rapid.Task

import java.net.InetAddress
import scala.util.Try

trait DNS {
  def lookup(hostName: String): Task[Option[IP]]
  /** All candidates in resolver order. Existing single-address resolvers remain compatible. */
  def lookupAll(hostName: String): Task[List[IP]] = lookup(hostName).map(_.toList)
}

object DNS {
  object default extends DNS {
    override def lookup(hostName: String): Task[Option[IP]] = lookupAll(hostName).map(_.headOption)
    override def lookupAll(hostName: String): Task[List[IP]] = Task {
      Try(InetAddress.getAllByName(hostName).toList.flatMap { a =>
        val scope = a.getHostAddress.split("%", 2).lift(1).filter(_.nonEmpty)
        IP.fromBytes(a.getAddress, scope)
      }).getOrElse(Nil)
    }
  }

  def apply(overrides: Map[String, IP]): DNS = {
    val map = overrides.map {
      case (hostName, ip) => hostName.toLowerCase -> ip
    }
    new DNS {
      override def lookup(hostName: String): Task[Option[IP]] = lookupAll(hostName).map(_.headOption)
      override def lookupAll(hostName: String): Task[List[IP]] = map.get(hostName.toLowerCase) match {
        case Some(ip) => Task.pure(List(ip))
        case None => default.lookupAll(hostName)
      }
    }
  }
}
