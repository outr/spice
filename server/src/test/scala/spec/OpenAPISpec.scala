package spec

import cats.effect.unsafe.implicits.global
import fabric.filter.RemoveNullsFilter
import fabric.io.{JsonFormatter, YamlFormatter}
import fabric.rw.Convertible
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.server.openapi.{OpenAPI, OpenAPIInfo, OpenAPIPath, OpenAPIResponse}
import spice.streamer.Streamer

import scala.collection.mutable

class OpenAPISpec extends AnyWordSpec with Matchers {
  "OpenAPI Generation" should {
    "create a minimal OpenAPI document manually" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "A minimal OpenAPI document",
          version = "0.0.1"
        )
      )
      val expected = Streamer(
        getClass.getClassLoader.getResourceAsStream("openapi-minimal.json"),
        new mutable.StringBuilder
      ).unsafeRunSync().toString
      api.asJsonString should be(expected)
    }
    "create a tic tac toe example manually" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Tic Tac Toe",
          version = "1.0.0",
          description = Some("This API allows writing down marks on a Tic Tac Toe board and requesting the state of the board or of individual squares.")
        ),
        paths = Map(
          "/board" -> Map(
            "get" -> OpenAPIPath(
              summary = "Get the whole board",
              description = "Retrieves the current state of the board and the winner.",
              responses = Map(
                "200" -> OpenAPIResponse(
                  description = "OK"
                )
              )
            )
          )
        )
      )
      println(api.asJsonString)
      // TODO: Should map to openapi-tictactoe.json
    }
  }
}
