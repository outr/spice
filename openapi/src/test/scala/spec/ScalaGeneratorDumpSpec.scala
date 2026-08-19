package spec

import fabric.io.YamlParser
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.OpenAPI
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.scala.OpenAPIScalaGenerator

import java.nio.file.{Files, Path}
import scala.io.Source

/** Writes the generated Scala to disk so the output can be read and compiled by hand. Skipped unless
  * `SCALA_GEN_DUMP` names a target directory, so it never writes during an ordinary test run. */
class ScalaGeneratorDumpSpec extends AnyWordSpec with Matchers {
  private def load(name: String): String = {
    val s = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name), "UTF-8")
    try s.mkString finally s.close()
  }

  "the generator" should {
    "dump generated sources when asked" in {
      sys.env.get("SCALA_GEN_DUMP") match {
        case None => succeed
        case Some(dir) =>
          val api = YamlParser(load("motn-openapi.yaml")).as[OpenAPI]
          val files = OpenAPIScalaGenerator(api, OpenAPIGeneratorConfig(), basePackage = "motn").generate()
          val out = Path.of(dir)
          Files.createDirectories(out)
          files.foreach(f => Files.writeString(out.resolve(f.fileName), f.source))
          info(s"wrote ${files.size} files to $out")
          succeed
      }
    }
  }
}
