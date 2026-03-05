package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.api.{ApiClient, ApiError}
import spice.api.server.ApiServer
import spice.http.client.HttpClient
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.net.*

// Shared types
case class User(id: String, name: String)
object User {
  given rw: RW[User] = RW.gen
}

case class CreateUserRequest(name: String, email: String)
object CreateUserRequest {
  given rw: RW[CreateUserRequest] = RW.gen
}

case class SearchResults(items: List[String], totalCount: Int)
object SearchResults {
  given rw: RW[SearchResults] = RW.gen
}

// Shared API trait
trait UserApi {
  def getUser(id: String): Task[User]
  def search(query: String, page: Int): Task[SearchResults]
  def createUser(request: CreateUserRequest): Task[User]
  def health(): Task[String]
}

// Server implementation
class UserApiImpl extends UserApi {
  override def getUser(id: String): Task[User] =
    Task.pure(User(id, "Alice"))

  override def search(query: String, page: Int): Task[SearchResults] =
    Task.pure(SearchResults(List(s"result-$query-$page"), 1))

  override def createUser(request: CreateUserRequest): Task[User] =
    Task.pure(User("generated-id", request.name))

  override def health(): Task[String] =
    Task.pure("ok")
}

class ApiSpec extends AnyWordSpec with Matchers {
  "ApiSpec" should {
    object server extends MutableHttpServer
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

    "configure and start server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      ApiServer.mount[UserApi](new UserApiImpl, server, path"/api")
      server.start().sync()
      server.isRunning should be(true)
    }

    "call health (zero params → GET)" in {
      val api = ApiClient.derive[UserApi](url"http://localhost".withPort(serverPort).withPath(path"/api"))
      api.health().map { result =>
        result should be("ok")
      }.sync()
    }

    "call getUser (single primitive param)" in {
      val api = ApiClient.derive[UserApi](url"http://localhost".withPort(serverPort).withPath(path"/api"))
      api.getUser("123").map { user =>
        user.id should be("123")
        user.name should be("Alice")
      }.sync()
    }

    "call search (multi-param)" in {
      val api = ApiClient.derive[UserApi](url"http://localhost".withPort(serverPort).withPath(path"/api"))
      api.search("scala", 2).map { results =>
        results.items should be(List("result-scala-2"))
        results.totalCount should be(1)
      }.sync()
    }

    "call createUser (single case class param)" in {
      val api = ApiClient.derive[UserApi](url"http://localhost".withPort(serverPort).withPath(path"/api"))
      api.createUser(CreateUserRequest("Bob", "bob@test.com")).map { user =>
        user.id should be("generated-id")
        user.name should be("Bob")
      }.sync()
    }

    "stop the server" in {
      server.stop().map { _ =>
        server.isRunning should be(false)
      }.sync()
    }
  }
}
