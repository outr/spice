package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.HttpStatus

class HttpStatusSpec extends AnyWordSpec with Matchers {
  "HttpStatus" should {
    "have correct code and message for common statuses" in {
      HttpStatus.OK.code should be(200)
      HttpStatus.OK.message should be("OK")
      HttpStatus.NotFound.code should be(404)
      HttpStatus.NotFound.message should be("Not Found")
      HttpStatus.InternalServerError.code should be(500)
      HttpStatus.InternalServerError.message should be("Internal Server Error")
      HttpStatus.MovedPermanently.code should be(301)
      HttpStatus.MovedPermanently.message should be("Moved Permanently")
    }
    "identify informational statuses with isInformation" in {
      HttpStatus.Continue.isInformation should be(true)
      HttpStatus.SwitchingProtocols.isInformation should be(true)
      HttpStatus.OK.isInformation should be(false)
    }
    "identify success statuses with isSuccess" in {
      HttpStatus.OK.isSuccess should be(true)
      HttpStatus.Created.isSuccess should be(true)
      HttpStatus.NoContent.isSuccess should be(true)
      HttpStatus.NotFound.isSuccess should be(false)
      HttpStatus.InternalServerError.isSuccess should be(false)
      HttpStatus.MovedPermanently.isSuccess should be(false)
    }
    "identify redirect statuses with isRedirection" in {
      HttpStatus.MovedPermanently.isRedirection should be(true)
      HttpStatus.Found.isRedirection should be(true)
      HttpStatus.SeeOther.isRedirection should be(true)
      HttpStatus.TemporaryRedirect.isRedirection should be(true)
      HttpStatus.OK.isRedirection should be(false)
      HttpStatus.NotFound.isRedirection should be(false)
    }
    "identify client error statuses with isClientError" in {
      HttpStatus.BadRequest.isClientError should be(true)
      HttpStatus.Unauthorized.isClientError should be(true)
      HttpStatus.Forbidden.isClientError should be(true)
      HttpStatus.NotFound.isClientError should be(true)
      HttpStatus.OK.isClientError should be(false)
      HttpStatus.InternalServerError.isClientError should be(false)
    }
    "identify server error statuses with isServerError" in {
      HttpStatus.InternalServerError.isServerError should be(true)
      HttpStatus.BadGateway.isServerError should be(true)
      HttpStatus.ServiceUnavailable.isServerError should be(true)
      HttpStatus.OK.isServerError should be(false)
      HttpStatus.NotFound.isServerError should be(false)
    }
    "identify errors with isError (client or server)" in {
      HttpStatus.BadRequest.isError should be(true)
      HttpStatus.NotFound.isError should be(true)
      HttpStatus.InternalServerError.isError should be(true)
      HttpStatus.BadGateway.isError should be(true)
      HttpStatus.OK.isError should be(false)
      HttpStatus.MovedPermanently.isError should be(false)
    }
    "look up statuses by code with byCode" in {
      HttpStatus.byCode(200) should be(HttpStatus.OK)
      HttpStatus.byCode(404) should be(HttpStatus.NotFound)
      HttpStatus.byCode(500) should be(HttpStatus.InternalServerError)
      HttpStatus.byCode(301) should be(HttpStatus.MovedPermanently)
    }
    "look up statuses by code with getByCode" in {
      HttpStatus.getByCode(200) should be(Some(HttpStatus.OK))
      HttpStatus.getByCode(404) should be(Some(HttpStatus.NotFound))
      HttpStatus.getByCode(999) should be(None)
    }
    "throw an exception for unknown codes via byCode" in {
      an[Exception] should be thrownBy HttpStatus.byCode(999)
    }
    "compare by code value using Ordered" in {
      HttpStatus.OK should be < HttpStatus.NotFound
      HttpStatus.NotFound should be < HttpStatus.InternalServerError
      HttpStatus.InternalServerError should be > HttpStatus.OK
    }
    "use toString to display code and message" in {
      HttpStatus.OK.toString should be("200: OK")
      HttpStatus.NotFound.toString should be("404: Not Found")
    }
    "allow equality based on code only" in {
      HttpStatus(200, "OK") should equal(HttpStatus(200, "Custom OK"))
    }
    "allow creating a copy with a different message" in {
      val custom = HttpStatus.OK("All Good")
      custom.code should be(200)
      custom.message should be("All Good")
    }
  }
}
