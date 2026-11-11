package spice.http.client

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.ConnectionStatus
import spice.net.{DNS, IP, URL}

import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, UnknownHostException}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class OkHttpDNSSpec extends AnyWordSpec with Matchers {
  moduload.Moduload.load()
  private def candidates(ips: IP*): DNS = new DNS {
    def lookup(host: String): Task[Option[IP]] = Task.pure(ips.headOption)
    override def lookupAll(host: String): Task[List[IP]] = Task.pure(ips.toList)
  }

  private def headers(socket: Socket): List[String] = {
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(socket.getInputStream, UTF_8))
    Iterator.continually(reader.readLine()).takeWhile(line => line != null && line.nonEmpty).toList
  }

  private def server(address: String)(serve: Socket => Unit)(test: Int => Unit): Unit = {
    val listener = new ServerSocket()
    listener.bind(new InetSocketAddress(InetAddress.getByName(address), 0))
    val worker = new Thread(() => {
      try {
        val socket = listener.accept()
        socket.setSoTimeout(5000)
        try serve(socket) finally socket.close()
      } catch { case _: java.net.SocketException if listener.isClosed => () }
    }, "spice-dns-test")
    worker.setDaemon(true)
    worker.start()
    try test(listener.getLocalPort) finally { listener.close(); worker.join(6000) }
  }

  "OkHttp DNS" should {
    "delegate default DNS directly to the platform resolver" in {
      (OkHttpDNS(DNS.default) eq okhttp3.Dns.SYSTEM) shouldBe true
      OkHttpDNS(DNS.default).lookup("localhost").asScala.toList shouldBe InetAddress.getAllByName("localhost").toList
    }
    "preserve IPv6, DNS64 and alternate IPv4 candidates from custom resolvers" in {
      val ips = List(IP.fromString("2606:4700:3031:0:0:0:ac43:d73e").get,
        IP.fromString("64:ff9b:0:0:0:0:c000:201").get, IP.v4(192, 0, 2, 255))
      val found = OkHttpDNS(candidates(ips*)).lookup("test.invalid").asScala.toList
      found.map(_.getAddress.toList) shouldBe ips.map(_.bytes.toList)
    }
    "retain compatibility with single-address custom resolvers and report missing hosts" in {
      val single: DNS = _ => Task.pure(Some(IP.v4.LocalHost))
      OkHttpDNS(single).lookup("test.invalid").get(0).getHostAddress shouldBe "127.0.0.1"
      intercept[UnknownHostException](OkHttpDNS(candidates()).lookup("test.invalid"))
    }
    "connect to a real IPv6 HTTP server using a hostname override" in {
      server("::1") { socket =>
        headers(socket)
        socket.getOutputStream.write("HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok".getBytes(UTF_8))
        socket.getOutputStream.flush()
      } { port =>
        val client = HttpClient.copy(implementation = OkHttpClientImplementation)
          .dns(DNS(Map("ipv6.test" -> IP.v6.LocalHost))).url(URL.parse(s"http://ipv6.test:$port/")).timeout(3.seconds)
        client.get.send().sync().content.get.asString.sync() shouldBe "ok"
      }
    }
    "try another address when the first route refuses the connection" in {
      server("127.0.0.1") { socket =>
        headers(socket)
        socket.getOutputStream.write("HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok".getBytes(UTF_8))
        socket.getOutputStream.flush()
      } { port =>
        val dns = candidates(IP.v4(127, 0, 0, 2), IP.v4.LocalHost)
        val client = HttpClient.copy(implementation = OkHttpClientImplementation)
          .dns(dns).url(URL.parse(s"http://fallback.test:$port/")).timeout(2.seconds)
        client.get.send().sync().content.get.asString.sync() shouldBe "ok"
      }
    }
    "open a real WebSocket over IPv6" in {
      server("::1") { socket =>
        val key = headers(socket).find(_.toLowerCase.startsWith("sec-websocket-key:")).get.split(":", 2)(1).trim
        val accept = Base64.getEncoder.encodeToString(MessageDigest.getInstance("SHA-1").digest((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes(UTF_8)))
        socket.getOutputStream.write(s"HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Accept: $accept\r\n\r\n".getBytes(UTF_8))
        socket.getOutputStream.flush()
        socket.getInputStream.read()
      } { port =>
        val client = new okhttp3.OkHttpClient.Builder().dns(OkHttpDNS(candidates(IP.v6.LocalHost))).build()
        val socket = new OkHttpWebSocket(URL.parse(s"ws://ipv6.test:$port/"), client)
        try socket.connect().timeout(3.seconds).sync() shouldBe ConnectionStatus.Open
        finally { socket.disconnect(); client.dispatcher.executorService.shutdown(); client.connectionPool.evictAll() }
      }
    }
    "cancel a pending WebSocket upgrade instead of leaving its connection open" in {
      val accepted = new CountDownLatch(1)
      val ended = new CountDownLatch(1)
      server("127.0.0.1") { socket =>
        headers(socket)
        accepted.countDown()
        if (socket.getInputStream.read() == -1) ended.countDown()
      } { port =>
        val client = new okhttp3.OkHttpClient.Builder().build()
        val socket = new OkHttpWebSocket(URL.parse(s"ws://127.0.0.1:$port/"), client)
        val connecting = socket.connect().start()
        try {
          accepted.await(3, TimeUnit.SECONDS) shouldBe true
          socket.disconnect()
          connecting.join.timeout(3.seconds).sync() shouldBe ConnectionStatus.Closed
          ended.await(3, TimeUnit.SECONDS) shouldBe true
        } finally { socket.disconnect(); client.dispatcher.executorService.shutdown(); client.connectionPool.evictAll() }
      }
    }
  }
}
