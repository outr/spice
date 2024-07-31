package spice.openapi.generator

import spice.openapi.OpenAPI

import java.io.File
import java.nio.file.{Files, Path}
import scala.io.Source

trait OpenAPIGenerator {
  protected def fileExtension: String
  protected def generatedComment: String

  def generate(api: OpenAPI, config: OpenAPIGeneratorConfig): List[SourceFile]

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

  def write(sourceFiles: List[SourceFile], path: Path, deleteBeforeWrite: Boolean = true): Unit = {
    if (deleteBeforeWrite) {
      sourceFiles.map(_.path).distinct.foreach { filePath =>
        val directory = path.resolve(filePath).toFile
        directory.listFiles().foreach { file =>
          if (isGenerated(file)) {
            if (!file.delete()) {
              file.deleteOnExit()
            }
          }
        }
      }
    }
    sourceFiles.foreach { sf =>
      val filePath = path.resolve(s"${sf.path}/${sf.fileName}")
      Files.createDirectories(filePath.getParent)
      Files.writeString(filePath, sf.source)
    }
  }
}