package spec

import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spec.TestUtils._
import spice.http.HttpMethod
import spice.net._
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIParameter, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema, OpenAPIServer, OpenAPITag, OpenAPIValue}

class OpenAPISpec extends AnyWordSpec with Matchers {
  "OpenAPI Generation" should {
    "create a minimal OpenAPI document manually" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "A minimal OpenAPI document",
          version = "0.0.1"
        )
      )
      val expected = loadJson("openapi-minimal.json")
      api.asJson should be(expected)
    }
    "create a simple OpenAPI document manually" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Sample API",
          description = Some("Optional multiline or single-line description in [CommonMark](https://commonmark.org/help/) or HTML."),
          version = "0.1.9"
        ),
        servers = List(
          OpenAPIServer(url = url"https://api.example.com/v1", description = Some("Optional server description, e.g. Main (production) server")),
          OpenAPIServer(url = url"https://staging-api.example.com", description = Some("Optional server description, e.g. Internal staging server for testing"))
        ),
        paths = Map(
          "/users" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Get -> OpenAPIPathEntry(
                summary = "Returns a list of users.",
                description = "Optional extended description in CommonMark or HTML.",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "A JSON array of user names",
                    content = OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Component(
                          `type` = "array",
                          items = Some(OpenAPISchema.Component(
                            `type` = "string"
                          ))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      val expected = loadString("openapi-simple.yml")
      api.asYaml should be(expected)
    }
    "create a simple OpenAPI document manually using oneOf" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Sample API",
          description = Some("Optional multiline or single-line description in [CommonMark](https://commonmark.org/help/) or HTML."),
          version = "0.1.9"
        ),
        paths = Map(
          "/poly" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Post -> OpenAPIPathEntry(
                summary = "Polymorphic",
                description = "Example",
                requestBody = Some(OpenAPIRequestBody(
                  required = true,
                  content = OpenAPIContent(
                    ContentType.`application/json` -> OpenAPIContentType(
                      schema = OpenAPISchema.OneOf(List(
                        OpenAPISchema.Component(
                          `type` = "string"
                        ),
                        OpenAPISchema.Component(
                          `type` = "integer"
                        )
                      ), nullable = None)
                    )
                  )
                )),
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "A JSON array of user names",
                    content = OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Component(
                          `type` = "array",
                          items = Some(OpenAPISchema.Component(
                            `type` = "string"
                          ))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
        )
      )
      val expected = loadString("openapi-poly.yml")
      api.asYaml should be(expected)
    }
    "create a tic tac toe example manually" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Tic Tac Toe",
          version = "1.0.0",
          description = Some("This API allows writing down marks on a Tic Tac Toe board and requesting the state of the board or of individual squares.")
        ),
        tags = List(
          OpenAPITag("Gameplay")
        ),
        paths = Map(
          "/board" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Get -> OpenAPIPathEntry(
                summary = "Get the whole board",
                description = "Retrieves the current state of the board and the winner.",
                tags = List(
                  "Gameplay"
                ),
                operationId = Some("get-board"),
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref(ref = "#/components/schemas/status")
                      )
                    )
                  )
                )
              )
            )
          ),
          "/board/{row}/{column}" -> OpenAPIPath(
            parameters = List(
              OpenAPISchema.Ref("#/components/parameters/rowParam"),
              OpenAPISchema.Ref("#/components/parameters/columnParam")
            ),
            methods = Map(
              HttpMethod.Get -> OpenAPIPathEntry(
                summary = "Get a single board square",
                description = "Retrieves the requested square.",
                tags = List("Gameplay"),
                operationId = Some("get-square"),
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/mark")
                      )
                    )
                  ),
                  "400" -> OpenAPIResponse(
                    description = "The provided parameters are incorrect",
                    content = OpenAPIContent(
                      ContentType.`text/html` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/errorMessage"),
                        example = Some("Illegal coordinates")
                      )
                    )
                  )
                )
              ),
              HttpMethod.Put -> OpenAPIPathEntry(
                summary = "Set a single board square",
                description = "Places a mark on the board and retrieves the whole board and the winner (if any).",
                tags = List("Gameplay"),
                operationId = Some("put-square"),
                requestBody = Some(OpenAPIRequestBody(
                  required = true,
                  content = OpenAPIContent(
                    ContentType.`application/json` -> OpenAPIContentType(
                      schema = OpenAPISchema.Ref("#/components/schemas/mark")
                    )
                  )
                )),
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/status")
                      )
                    )
                  ),
                  "400" -> OpenAPIResponse(
                    description = "The provided parameters are incorrect",
                    content = OpenAPIContent(
                      ContentType.`text/html` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/errorMessage"),
                        examples = Map(
                          "illegalCoordinates" -> OpenAPIValue("Illegal coordinates."),
                          "notEmpty" -> OpenAPIValue("Square is not empty."),
                          "invalidMark" -> OpenAPIValue("Invalid Mark (X or O).")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        components = Some(OpenAPIComponents(
          parameters = Map(
            "rowParam" -> OpenAPIParameter(
              description = "Board row (vertical coordinate)",
              name = "row",
              in = "path",
              required = true,
              schema = OpenAPISchema.Ref("#/components/schemas/coordinate")
            ),
            "columnParam" -> OpenAPIParameter(
              description = "Board column (horizontal coordinate)",
              name = "column",
              in = "path",
              required = true,
              schema = OpenAPISchema.Ref("#/components/schemas/coordinate")
            )
          ),
          schemas = Map(
            "errorMessage" -> OpenAPISchema.Component(
              `type` = "string",
              maxLength = Some(256),
              description = Some("A text message describing an error")
            ),
            "coordinate" -> OpenAPISchema.Component(
              `type` = "integer",
              minimum = Some(1),
              maximum = Some(3),
              example = Some(1)
            ),
            "mark" -> OpenAPISchema.Component(
              `type` = "string",
              `enum` = List(
                ".",
                "X",
                "O"
              ),
              description = Some("Possible values for a board square. `.` means empty square."),
              example = Some(".")
            ),
            "board" -> OpenAPISchema.Component(
              `type` = "array",
              maxItems = Some(3),
              minItems = Some(3),
              items = Some(OpenAPISchema.Component(
                `type` = "array",
                maxItems = Some(3),
                minItems = Some(3),
                items = Some(OpenAPISchema.Ref("#/components/schemas/mark"))
              ))
            ),
            "winner" -> OpenAPISchema.Component(
              `type` = "string",
              `enum` = List(
                ".",
                "X",
                "O"
              ),
              description = Some("Winner of the game. `.` means nobody has won yet."),
              example = Some(".")
            ),
            "status" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "winner" -> OpenAPISchema.Ref("#/components/schemas/winner"),
                "board" -> OpenAPISchema.Ref("#/components/schemas/board")
              )
            )
          )
        ))
      )
      val expected = loadJson("openapi-tictactoe.json")
      api.asJson should be(expected)
    }
  }
}
