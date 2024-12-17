package spice.net

import rapid.Task

import java.net.InetAddress
import scala.util.Try

trait DNS {
  def lookup(hostName: String): Task[Option[IP]]
}

object DNS {
  object default extends DNS {
    override def lookup(hostName: String): Task[Option[IP]] = Task {
      Try(Option(InetAddress.getByName(hostName)).map { a =>
        val b = a.getAddress
        IP.v4(b(0), b(1), b(2), b(3))
      }).getOrElse(None)
    }
  }

  def apply(overrides: Map[String, IP]): DNS = {
    val map = overrides.map {
      case (hostName, ip) => hostName.toLowerCase -> ip
    }
    (hostName: String) => Task(map.get(hostName.toLowerCase)).flatMap {
      case s: Some[IP] => Task.pure(s)
      case _ => default.lookup(hostName)
    }
  }
}