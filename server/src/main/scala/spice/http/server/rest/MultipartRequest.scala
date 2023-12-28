package spice.http.server.rest

import fabric.rw._
import spice.http.content.FormDataContent

/**
 * Drop-in convenience class to wrap around an existing Request object and give back `FormDataContent` when available.
 */
case class MultipartRequest[Request](request: Request,
                                     content: Option[FormDataContent])

object MultipartRequest {
  implicit def rw[Request: RW]: RW[MultipartRequest[Request]] = RW.from[MultipartRequest[Request]](
    r = req => req.request.json,
    w = json => MultipartRequest[Request](
      request = json.as[Request],
      content = None
    ),
    d = implicitly[RW[Request]].definition
  )
}