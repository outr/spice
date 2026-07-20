package spice.openapi.generator

import spice.openapi.OpenAPI

import java.io.File
import java.nio.file.{Files, Path}
import scala.io.Source

trait OpenAPIGenerator {
  def api: OpenAPI
  def config: OpenAPIGeneratorConfig

  protected def fileExtension: String
  protected def generatedComment: String

  def generate(): List[SourceFile]

  protected def isGenerated(file: File): Boolean = if (!file.isDirectory && file.getName.toLowerCase.endsWith(fileExtension)) {
    val s = Source.fromFile(file)
    try {
      s.getLines().exists(_.contains(generatedComment))
    } finally {
      s.close()
    }
  } else {
    false
  }

  def write(sourceFiles: List[SourceFile], path: Path, deleteBeforeWrite: Boolean = true): Unit =
    GeneratedSourceWriter.write(sourceFiles, path, fileExtension, generatedComment, deleteBeforeWrite)
}