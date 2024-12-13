package spice.delta

import spice.http.content.{Content, FileContent, StringContent, URLContent}
import spice.streamer._

import java.io.{File, FileInputStream, InputStream}
import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object HTMLParser {
  private val SelfClosingTagRegex = """(?s)<(\S+)(.*)/>""".r
  private val OpenTagRegex = """(?s)<(\S+)(.*)>""".r
  private val CloseTagRegex = """(?s)</(\S+).*>""".r

  /**
    * If set to true the stored tag attributes will be limited to those in validAttributes.
    *
    * Defaults to false.
    */
  var filterAttributes: Boolean = false
  /**
    * The set of attributes to limit to if filterAttributes is set to true.
    *
    * Defaults to "id", "class", and "data-spice".
    */
  var validAttributes: Set[String] = Set("id", "class", "data-spice", "data-spice-class")

  private val parsers = new ConcurrentHashMap[File, StreamableHTML]().asScala

  def cache(content: Content): StreamableHTML = content match {
    case FileContent(file, _, _) => cache(file)
    case URLContent(url, _, _) => cache(url)
    case StringContent(value, _, _) => cache(value)
  }

  def cache(file: File): StreamableHTML = parsers.getOrElseUpdate(file, HTMLParser(file))

  def cache(url: URL): StreamableHTML = {
    val file = File.createTempFile("htmlparser", "cache")
    file.deleteOnExit()
    Streamer(url, file).sync()
    cache(file)
  }

  def cache(html: String): StreamableHTML = {
    val file = File.createTempFile("htmlparser", "cache")
    file.deleteOnExit()
    Streamer(html, file).sync()
    cache(file)
  }

  def apply(file: File): StreamableHTML = {
    val cacheBuilder: CacheBuilder = new CacheBuilder {
      private var lastModified: Long = 0L

      override def isStale: Boolean = lastModified != file.lastModified()

      override def buildCache(): CachedInformation = {
        val input = new FileInputStream(file)
        try {
          val parser = new HTMLParser(input)
          val tags = parser.parse()
          var byId = Map.empty[String, Tag.Open]
          var byClass = Map.empty[String, Set[Tag.Open]]
          var byTag = Map.empty[String, Set[Tag.Open]]
          var byAttribute = Map.empty[String, Set[Tag.Open]]
          tags.foreach { tag =>
            if (tag.attributes.contains("id")) {
              byId += tag.attributes("id") -> tag
            }
            tag.attributes.getOrElse("class", "").split(" ").foreach { className =>
              val cn = className.trim
              if (cn.nonEmpty) {
                var classTags = byClass.getOrElse(cn, Set.empty[Tag.Open])
                classTags += tag
                byClass += cn -> classTags
              }
            }
            tag.attributes.keys.foreach { attributeName =>
              var attributeNameTags = byAttribute.getOrElse(attributeName, Set.empty[Tag.Open])
              attributeNameTags += tag
              byAttribute += attributeName -> attributeNameTags
            }
            var tagsByName = byTag.getOrElse(tag.tagName, Set.empty[Tag.Open])
            tagsByName += tag
            byTag += tag.tagName -> tagsByName
          }
          lastModified = file.lastModified()
          CachedInformation(byId, byClass, byTag, byAttribute)
        } catch {
          case throwable: Throwable => throw new RuntimeException(s"Error parsing ${file.getAbsolutePath}", throwable)
        } finally {
          input.close()
        }
      }
    }
    new StreamableHTML(file, cacheBuilder)
  }
}

class HTMLParser(input: InputStream) {
  import HTMLParser._

  private var position = 0
  private val b = new StringBuilder
  private var quotes = false
  private var tagStart = -1
  private var tagEnd = -1

  private var open = List.empty[Tag.Open]
  private var tags = Set.empty[Tag.Open]

  @tailrec
  final def parse(): Set[Tag.Open] = input.read() match {
    case -1 => tags   // Finished
    case i => {
      val c = i.toChar
      process(c)
      parse()
    }
  }

  private def process(c: Char): Unit = {
    b.append(c)
    if (c == '"') {
      if (quotes) {
        quotes = false
      } else {
        quotes = true
      }
    } else if (c == '\n' || c == '\r') {
      quotes = false
    } else if (c == '<' && !quotes) {
      b.clear()
      b.append(c)
      tagStart = position
    } else if (tagStart != -1 && c == '>' && !quotes) {
      tagEnd = position + 1
      parseTag()
      b.clear()
      tagStart = -1
    }

    position += 1
  }

  private def parseTag(): Unit = b.toString match {
    case s if s.startsWith("<!--") || s.endsWith("-->") => // Ignore
    case CloseTagRegex(tagName) => {
      val closeTag = Tag.Close(tagName, tagStart, tagEnd)
      val openTag = closeUntil(tagName).copy(close = Some(closeTag))
      tags += openTag
    }
    case SelfClosingTagRegex(tagName, attributes) => {
      val a = parseAttributes(attributes)
      val tag = Tag.Open(tagName, a, tagStart, tagEnd, close = None)
      tags += tag
    }
    case OpenTagRegex(tagName, attributes) => {
      val a = parseAttributes(attributes)
      val tag = Tag.Open(tagName, a, tagStart, tagEnd, close = None)
      open = tag :: open
    }
  }

  private def parseAttributes(attributes: String): Map[String, String] = {
    val sb = new StringBuilder
    var quoted = false
    var key = ""
    var map = Map.empty[String, String]
    attributes.zipWithIndex.foreach {
      case (c, index) => {
        if (c == '"') {
          if (quoted) {
            map += key -> sb.toString
            quoted = false
            sb.clear()
          } else {
            quoted = true
          }
        } else if ((c == '=' || c == ' ') && !quoted) {
          if (sb.nonEmpty) {
            key = sb.toString
            sb.clear()
          }
        } else {
          sb.append(c)
        }
      }
    }
    if (filterAttributes) {
      map.filter(t => validAttributes.contains(t._1))
    } else {
      map
    }
  }

  @tailrec
  private def closeUntil(tagName: String): Tag.Open = {
    if (open.isEmpty) {
      throw new RuntimeException(s"Missing close tag for $tagName!")
    }
    val t = open.head
    open = open.tail
    if (t.tagName.equalsIgnoreCase(tagName)) {
      t
    } else {
      tags += t
      closeUntil(tagName)
    }
  }
}