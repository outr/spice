package spice.openapi.server

import rapid._
import fabric.rw._
import spice.net.URLPath

case class TypedRestService[Req, Res](path: URLPath,
                                      summary: String,
                                      override val responseTypes: List[ResponseType],
                                      f: Req => Task[Res])
                                     (implicit val requestRW: RW[Req], val responseRW: RW[Res]) extends RestService {
  override type Request = Req
  override type Response = Res

  override protected def apply(request: Req): Task[Res] = f(request)
}
