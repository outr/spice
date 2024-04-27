package spice.openapi.server

import cats.effect.IO
import fabric.rw.RW
import spice.net.URLPath

case class TypedRestService[Req, Res](path: URLPath,
                                      summary: String,
                                      override val responseTypes: List[ResponseType],
                                      f: Req => IO[Res])
                                     (implicit val requestRW: RW[Req], val responseRW: RW[Res]) extends RestService {
  override type Request = Req
  override type Response = Res

  override protected def apply(request: Req): IO[Res] = f(request)
}
