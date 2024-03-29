package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.delta.{HTMLParser, Selector}
import spice.delta.types.Delta

class TemplateGeneratorSpec extends AnyWordSpec with Matchers {
  "TemplateGenerator" should {
    "generate source from a simple HTML file" in {
      val html = HTMLParser.cache(
        """<html>
          |<head>
          |</head>
          |<body>
          |<h1 data-spice="object" id="heading">Heading</h1>
          |<ul>
          | <li data-spice="class" data-spice-class="listItem" id="entry">Entry</li>
          |</ul>
          |</body>
          |</html>""".stripMargin)
      val result = html.stream(deltas = List(
        Delta.Process(
          selector = Selector.HasAtribute("data-spice"),
          replace = true,
          onlyOpenTag = false,
          processor = (tag, content) => {
            val `type` = tag.attributes("data-spice")
            val id = tag.attributes.getOrElse("id", throw new RuntimeException(s"No id defined for: $content"))
            val className = tag.attributes.getOrElse("data-spice-class", id)
            scribe.info(s"Tag: $tag, Content: $content, Type: ${`type`}, Class: $className")
            "REPLACED"
          },
          closeTagProcessor = None
        )
      ))
      result.replaceAll("\\s", "") should be(
        "<html><head></head><body>REPLACED<ul>REPLACED</ul></body></html>"
      )
    }
  }
}