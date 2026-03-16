package spice.api.server

import fabric.rw.RW

case class ApiParamDescriptor(name: String, rw: RW[?])

case class ApiMethodDescriptor(
  name: String,
  httpMethod: String,
  params: List[ApiParamDescriptor],
  requestRW: Option[RW[?]],
  responseRW: RW[?]
)
