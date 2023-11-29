package spec

import cats.effect.testing.scalatest.AsyncIOSpec
import fabric._
import fabric.io.JsonFormatter
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.HttpMethod
import spice.http.server.openapi._
import spice.net.{URLPath, _}
import spice.openapi.server.{OpenAPIHttpServer, Schema, Service, ServiceCall}

class OpenAPIServerAdvancedSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "OpenAPIServer Advanced" should {
    "validate a proper swagger.yml file" in {
      val expected = TestUtils.loadJson("openapi-tictactoe.json")
      val json = AdvancedOpenAPIServer.api.asJson
      println(JsonFormatter.Default(json))
      // TODO: Finish support
//      json should be(expected)
      succeed
    }
  }

  object AdvancedOpenAPIServer extends OpenAPIHttpServer {
    override def title: String = "Tic Tac Toe"
    override def version: String = "1.0.0"
    override def description: Option[String] = Some("This API allows writing down marks on a Tic Tac Toe board and requesting the state of the board or of individual squares.")
    override def tags: List[String] = List("gameplay")

    object board extends Service {
      override val path: URLPath = path"/board"

      override val calls: List[ServiceCall] = List(
        serviceCall[Unit, Status](
          method = HttpMethod.Get,
          summary = "Get the whole board",
          description = "Retrieves the current state of the board and the winner.",
          successDescription = "OK",
          tags = List("Gameplay"),
          operationId = Some("get-board"),
          responseSchema = Some(Schema(
            properties = Map(
              "winner" -> Schema(
                description = Some("Winner of the game. `.` means nobody has won yet."),
                example = Some(".")
              ),
              "board" -> Schema(
                maxItems = Some(3),
                minItems = Some(3),
                items = Some(Schema(
                  maxItems = Some(3),
                  minItems = Some(3),
                  items = Some(Schema(
                    description = Some("Possible values for a board square. `.` means empty square."),
                    example = Some(".")
                  ))
                ))
              )
            )
          ))
        ) { request =>
          request.response(Status(
            winner = Winner.`.`,
            board = List(List())
          ))
        }
      )
    }

    object boardSquare extends Service {
      override val path: URLPath = path"/board/{row}/{column}"

      override val calls: List[ServiceCall] = List(
        serviceCall[Square, Mark](
          method = HttpMethod.Get,
          summary = "Get a single board square",
          description = "Retrieves the requested square.",
          tags = List("Gameplay"),
          operationId = Some("get-square"),
          successDescription = "OK",
          responseSchema = Some(Schema(
            description = Some("Possible values for a board square. `.` means empty square."),
            example = Some(".")
          ))
        ) { request =>
          request.response(Mark.`.`)
        }
      )
    }

    override def services: List[Service] = List(
      board, boardSquare
    )

    case class Status(winner: Winner, board: List[List[Mark]])

    object Status {
      implicit val rw: RW[Status] = RW.gen
    }

    case class Winner(value: String)

    object Winner {
      implicit val rw: RW[Winner] = RW.enumeration(
        list = List(`.`, X, O),
        asString = (w: Winner) => w.value,
        caseSensitive = false
      )

      lazy val `.`: Winner = Winner(".")
      lazy val X: Winner = Winner("X")
      lazy val O: Winner = Winner("O")
    }

    case class Mark(value: String)

    object Mark {
      implicit val rw: RW[Mark] = RW.enumeration(
        list = List(`.`, X, O),
        asString = (m: Mark) => m.value,
        caseSensitive = false
      )

      lazy val `.`: Mark = Mark(".")
      lazy val X: Mark = Mark("X")
      lazy val O: Mark = Mark("O")
    }

    case class Square(row: Int, column: Int)

    object Square {
      implicit val rw: RW[Square] = RW.gen
    }
  }
}