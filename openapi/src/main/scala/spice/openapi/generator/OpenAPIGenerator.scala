package spice.openapi.generator

import spice.openapi.OpenAPI

import java.nio.file.{Files, Path}

trait OpenAPIGenerator {
  def generate(api: OpenAPI, config: OpenAPIGeneratorConfig): List[SourceFile]

  def write(sourceFiles: List[SourceFile], path: Path): Unit = sourceFiles.foreach { sf =>
    val filePath = path.resolve(s"${sf.path}/${sf.fileName}")
    Files.createDirectories(filePath.getParent)
    Files.writeString(filePath, sf.source)
  }
}