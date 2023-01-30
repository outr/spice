package spice.http.client

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import fabric.io.JsonFormatter
import okhttp3.{Authenticator, Request, Response, Route}
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}
import spice.http.content._
import spice.http._
import spice.net.ContentType
import spice.streamer._

import java.io.{File, IOException}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.util
import java.util.concurrent.TimeUnit
import javax.net.ssl._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * Asynchronous HttpClient for simple request response support.
 *
 * Adds support for simple restful request/response JSON support.
 */
class OkHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  private lazy val instance = {
    val b = new okhttp3.OkHttpClient.Builder()
    b.sslSocketFactory(new SSLSocketFactory {
      //      private val default = SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
      private val disabled = {
        val trustAllCerts = Array[TrustManager](new X509TrustManager {
          override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

          override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

          override def getAcceptedIssuers: Array[X509Certificate] = null
        })
        val sc = SSLContext.getInstance("SSL")
        sc.init(null, trustAllCerts, new SecureRandom)
        sc.getSocketFactory
      }

      private def f: SSLSocketFactory = disabled //if (config.validateSSLCertificates) default else disabled

      override def getDefaultCipherSuites: Array[String] = f.getDefaultCipherSuites

      override def getSupportedCipherSuites: Array[String] = f.getSupportedCipherSuites

      override def createSocket(socket: Socket, s: String, i: Int, b: Boolean): Socket = f.createSocket(socket, s, i, b)

      override def createSocket(s: String, i: Int): Socket = f.createSocket(s, i)

      override def createSocket(s: String, i: Int, inetAddress: InetAddress, i1: Int): Socket = f.createSocket(s, i, inetAddress, i1)

      override def createSocket(inetAddress: InetAddress, i: Int): Socket = f.createSocket(inetAddress, i)

      override def createSocket(inetAddress: InetAddress, i: Int, inetAddress1: InetAddress, i1: Int): Socket = f.createSocket(inetAddress, i, inetAddress1, i1)
    }, new X509TrustManager {
      //      private val factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      //      private val default = {
      //        factory.init(KeyStore.getInstance(KeyStore.getDefaultType))
      //        factory.getTrustManagers.apply(0).asInstanceOf[X509TrustManager]
      //      }

      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {
        if (client.validateSSLCertificates) {
          //          default.checkClientTrusted(x509Certificates, s)
        }
      }

      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {
        if (client.validateSSLCertificates) {
          //          default.checkServerTrusted(x509Certificates, s)
        }
      }

      override def getAcceptedIssuers: Array[X509Certificate] = //if (config.validateSSLCertificates) {
      //        default.getAcceptedIssuers
      //      } else {
        Array.empty[X509Certificate]
      //      }
    })
    b.hostnameVerifier(new HostnameVerifier {
      override def verify(s: String, sslSession: SSLSession): Boolean = true
    })
    b.connectTimeout(client.timeout.toMillis, TimeUnit.MILLISECONDS)
    b.readTimeout(client.timeout.toMillis, TimeUnit.MILLISECONDS)
    b.writeTimeout(client.timeout.toMillis, TimeUnit.MILLISECONDS)
    b.dns((hostname: String) => {
      val list = new util.ArrayList[InetAddress]()
      client.dns.lookup(hostname).unsafeRunSync() match {
        case Some(ip) => list.add(InetAddress.getByAddress(ip.address.map(_.toByte).toArray))
        case None => // None
      }
      list
    })
    client.pingInterval.foreach(d => b.pingInterval(d.toMillis, TimeUnit.MILLISECONDS))
    client.proxy.foreach {
      case Proxy(t, h, p, c) =>
        val `type` = t match {
          case ProxyType.Direct => java.net.Proxy.Type.DIRECT
          case ProxyType.Http => java.net.Proxy.Type.HTTP
          case ProxyType.Socks => java.net.Proxy.Type.SOCKS
        }
        val sa = new InetSocketAddress(h, p)
        b.proxy(new java.net.Proxy(`type`, sa))
        c.foreach { creds =>
          b.proxyAuthenticator(new Authenticator {
            override def authenticate(route: Route, response: Response): Request = {
              val credential = okhttp3.Credentials.basic(creds.username, creds.password)
              response.request().newBuilder().header("Proxy-Authorization", credential).build()
            }
          })
        }
    }
    b.build()
  }

  /*def disableSSLVerification(): Unit = {
    val trustAllCerts = Array[TrustManager](new X509TrustManager {
      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

      override def getAcceptedIssuers: Array[X509Certificate] = null
    })
    val sc = SSLContext.getInstance("SSL")
    sc.init(null, trustAllCerts, new SecureRandom)
    HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory)
    val allHostsValid = new HostnameVerifier {
      override def verify(s: String, sslSession: SSLSession): Boolean = true
    }
    HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid)
  }*/

  override def send(request: HttpRequest): IO[Try[HttpResponse]] = Deferred[IO, Try[HttpResponse]].flatMap { deferred =>
    val req = requestToOk(request)
    instance.newCall(req).enqueue(new okhttp3.Callback {
      override def onResponse(call: okhttp3.Call, res: okhttp3.Response): Unit = {
        val response = responseFromOk(res)
        deferred.complete(Success(response)).unsafeRunSync()
      }

      override def onFailure(call: okhttp3.Call, exc: IOException): Unit = {
        deferred.complete(Failure(exc)).unsafeRunSync()
      }
    })
    OkHttpClientImplementation.process(deferred.get)
  }

  private def requestToOk(request: HttpRequest): okhttp3.Request = {
    val r = new okhttp3.Request.Builder().url(request.url.toString)

    // Headers
    request.headers.map.foreach {
      case (key, values) => values.foreach(r.addHeader(key, _))
    }

    def ct(contentType: ContentType): okhttp3.MediaType = okhttp3.MediaType.parse(contentType.outputString)

    // Content
    val body = request.content.map {
      case StringContent(value, contentType, _) => okhttp3.RequestBody.create(value, ct(contentType))
      case FileContent(file, contentType, _) => okhttp3.RequestBody.create(file, ct(contentType))
      case BytesContent(array, contentType, _) => okhttp3.RequestBody.create(array, ct(contentType))
      case JsonContent(json, compact, contentType, _) =>
        val jsonString = if (compact) JsonFormatter.Compact(json) else JsonFormatter.Default(json)
        okhttp3.RequestBody.create(jsonString, ct(contentType))
      case FormDataContent(data) => {
        val form = new okhttp3.MultipartBody.Builder()
        form.setType(ct(ContentType.`multipart/form-data`))
        data.foreach {
          case FormData(key, entries) => entries.foreach {
            case StringEntry(value, _) => form.addFormDataPart(key, value)
            case FileEntry(fileName, file, headers) => {
              val partType = Headers.`Content-Type`.value(headers).getOrElse(ContentType.`application/octet-stream`)
              val part = okhttp3.RequestBody.create(file, ct(partType))
              form.addFormDataPart(key, fileName, part)
            }
          }
        }
        form.build()
      }
      case c => throw new RuntimeException(s"Unsupported request content: $c")
    }.getOrElse {
      if (request.method != HttpMethod.Get) {
        okhttp3.RequestBody.create("", None.orNull)
      } else {
        None.orNull
      }
    }

    // Method
    r
      .method(request.method.value, body)
      .header("Content-Length", Option(body).map(_.contentLength().toString).getOrElse("0"))
      .build()
  }

  private def responseFromOk(r: okhttp3.Response): HttpResponse = {
    // Status
    val status = HttpStatus(code = r.code(), message = r.message())

    // Headers
    val headersMap = r.headers().names().asScala.toList.map { key =>
      key -> r.headers(key).asScala.toList
    }.toMap
    val headers = Headers(headersMap)

    // Content
    val contentType = Headers.`Content-Type`.value(headers).getOrElse(ContentType.`application/octet-stream`)
    val contentLength = Headers.`Content-Length`.value(headers)
    val content = Option(r.body()).map { responseBody =>
      if (contentToString(contentType, contentLength)) {
        Content.string(responseBody.string(), contentType)
      } else if (contentToBytes(contentType, contentLength)) {
        Content.bytes(responseBody.bytes(), contentType)
      } else {
        val suffix = contentType.extension.getOrElse("client")
        val file = File.createTempFile("spice", s".$suffix", new File(client.saveDirectory))
        Streamer(responseBody.byteStream(), file).unsafeRunSync()
        Content.file(file, contentType)
      }
    }

    HttpResponse(
      status = status,
      headers = headers,
      content = content
    )
  }

  private def contentToString(contentType: ContentType, contentLength: Option[Long]): Boolean = {
    contentType.`type` == "text" || contentType.subType == "json"
  }

  private def contentToBytes(contentType: ContentType, contentLength: Option[Long]): Boolean = {
    contentLength.exists(l => l > 0L && l < 512000L)
  }

  override def dispose(): IO[Unit] = for {
    _ <- IO(Try(instance.dispatcher().executorService().shutdown()))
    _ <- IO(Try(instance.connectionPool().evictAll()))
    _ <- IO(Try(instance.cache().close()))
  } yield {
    ()
  }
}
