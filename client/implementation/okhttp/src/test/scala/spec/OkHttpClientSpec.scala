package spec

import rapid._
import fabric.rw._
import moduload.Moduload
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http._
import spice.http.client.intercept.Interceptor
import spice.http.client.{HttpClient, OkHttpClientImplementation}
import spice.http.content._
import spice.net._

import scala.concurrent.duration._

class OkHttpClientSpec extends AnyWordSpec with Matchers {
  "OkHttpClient" should {
    "load Moduload" in {
      Moduload.load()
    }
    "be the default implementation" in {
      HttpClient.implementation should be(OkHttpClientImplementation)
    }
    "GET the user-agent" in {
      HttpClient.url(url"https://httpbin.org/user-agent").get.send().map { response =>
        response.status should be(HttpStatus.OK)
        val content = response.content.get.asInstanceOf[StringContent]
        content.value.contains("user-agent") should be(true)
      }
    }
    "call a URL multiple times with a rate limiter" in {
      var calls = 0
      val limiter = Interceptor.rateLimited(1.seconds)

      def callMultiple(counter: Int): Task[Unit] = {
        HttpClient.interceptor(limiter).url(url"https://httpbin.org/user-agent").get.send().flatMap { response =>
          response.status should be(HttpStatus.OK)
          calls += 1
          if (counter > 0) {
            callMultiple(counter - 1)
          } else {
            IO.unit
          }
        }
      }

      val start = System.currentTimeMillis()
      callMultiple(5).map { _ =>
        calls should be(6)
        val elapsed = System.currentTimeMillis() - start
        elapsed <= 10000L should be(true)
      }
    }
    "call a URL and get a case class back" in {
      HttpClient.url(url"https://jsonplaceholder.typicode.com/todos/1").get.callTry[Placeholder].map(_.get).map { p =>
        p.userId should be(1)
        p.id should be(1)
        p.title should be("delectus aut autem")
        p.completed should be(false)
      }
    }
    "restful call to a URL" in {
      HttpClient
        .url(url"https://jsonplaceholder.typicode.com/posts")
        .restfulTry[Placeholder, Placeholder](Placeholder(123, 456, "Test YouI", completed = false)).map(_.get).map { p =>
        p.userId should be(123)
        p.id should be(101)
        p.title should be("Test YouI")
        p.completed should be(false)
      }
    }
    "call a URL and get an image back" in {
      val url = url"https://s.yimg.com/ny/api/res/1.2/8Qe5c2B.moDrzo4jn7T5VQ--~A/YXBwaWQ9aGlnaGxhbmRlcjt3PTU2MzI7aD0zNzU1O3NtPTE7aWw9cGxhbmU-/https://media-mbst-pub-ue1.s3.amazonaws.com/creatr-images/2020-04/81f62d40-7ff9-11ea-bfdd-25ac22907561.cf.jpg"
      HttpClient.url(url).send().map { response =>
        response.status should be(HttpStatus.OK)
        response.content.map(_.contentType) should be(Some(ContentType.`image/jpeg`))
      }
    }
  }

  case class Placeholder(userId: Int, id: Int, title: String, completed: Boolean)

  object Placeholder {
    implicit val rw: RW[Placeholder] = RW.gen
  }
}
