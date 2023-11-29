package spice.openapi.generator

import spice.openapi.OpenAPI

trait OpenAPIGenerator {
  def generate(api: OpenAPI): List[SourceFile]
}