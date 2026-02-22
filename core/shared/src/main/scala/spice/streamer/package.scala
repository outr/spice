package spice

import java.io.{ByteArrayInputStream, File, FileInputStream, FileOutputStream, IOException, InputStream, OutputStream}
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

package object streamer {
  class InputStreamReader(input: InputStream) extends Reader {
    override def length: Option[Long] = None

    override def read(buffer: Array[Byte]): Int = input.read(buffer)

    override def close(): Unit = input.close()
  }

  given Conversion[InputStream, InputStreamReader] = input => new InputStreamReader(input)

  given Conversion[java.io.Reader, Reader] = reader => new Reader {
    override def length: Option[Long] = None

    override def read(buffer: Array[Byte]): Int = {
      val b = new Array[Char](buffer.length)
      val len = reader.read(b)
      b.zipWithIndex.foreach {
        case (c, index) => buffer(index) = c.toByte
      }
      len
    }

    override def close(): Unit = reader.close()
  }

  given array2Reader: Conversion[Array[Byte], InputStreamReader] = array => new InputStreamReader(new ByteArrayInputStream(array))

  given file2Reader: Conversion[File, InputStreamReader] = file => new InputStreamReader(new FileInputStream(file)) {
    override def length: Option[Long] = Some(file.length())
  }

  given path2Reader: Conversion[Path, InputStreamReader] = path => file2Reader(path.toFile)

  given url2Reader: Conversion[URL, InputStreamReader] = url => urlInputStream(url, Set.empty)

  @tailrec
  private def urlInputStream(url: URL, redirects: Set[String]): InputStreamReader = {
    val exchange: URLConnection = url.openConnection()
    val redirect = exchange match {
      case c: HttpURLConnection => c.getResponseCode match {
        case code if Set(HttpURLConnection.HTTP_MOVED_TEMP, HttpURLConnection.HTTP_MOVED_PERM, HttpURLConnection.HTTP_SEE_OTHER).contains(code) => true
        case _ => false
      }
      case _ => false
    }
    if (!redirect) {
      val len = exchange.getContentLengthLong
      new InputStreamReader(exchange.getInputStream) {
        override def length: Option[Long] = len match {
          case _ if len < 0 => None
          case _ => Some(len)
        }
      }
    } else {
      val redirectURL = exchange.getHeaderField("Location")
      if (redirects.contains(redirectURL)) {
        throw new IOException(s"Redirect loop detected: ${redirects.mkString(", ")}")
      }
      scribe.warn(s"Download URL redirecting from $url to $redirectURL")
      urlInputStream(new URI(redirectURL).toURL, redirects + url.toString)
    }
  }

  given youiURL2Reader: Conversion[net.URL, InputStreamReader] = url => url2Reader(new URI(url.toString).toURL)

  given string2Reader: Conversion[String, InputStreamReader] = s => new InputStreamReader(new ByteArrayInputStream(s.getBytes)) {
    override def length: Option[Long] = Some(s.length)
  }

  class OutputStreamWriter(output: OutputStream) extends Writer {
    override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = output.write(buffer, offset, length)

    override def flush(): Unit = output.flush()

    override def close(): Unit = output.close()

    override def complete(): Unit = {}
  }

  given Conversion[OutputStream, OutputStreamWriter] = output => new OutputStreamWriter(output)

  given file2Writer: Conversion[File, OutputStreamWriter] = file => {
    val temp = new File(file.getParentFile, s".${file.getName}.temp")
    new OutputStreamWriter(new FileOutputStream(temp)) {
      override def complete(): Unit = {
        super.complete()

        file.delete()         // Make sure the original file doesn't exist
        temp.renameTo(file)   // Two-stage write to avoid partials
      }
    }
  }

  given path2Writer: Conversion[Path, OutputStreamWriter] = path => file2Writer(path.toFile)

  class StringBuilderWriter(sb: mutable.StringBuilder) extends Writer {
    override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = {
      sb.append(new String(buffer, offset, length))
    }

    override def flush(): Unit = {}

    override def close(): Unit = {}

    override def complete(): Unit = {}

    override def toString: String = sb.toString
  }

  given Conversion[mutable.StringBuilder, StringBuilderWriter] = sb => new StringBuilderWriter(sb)
}
