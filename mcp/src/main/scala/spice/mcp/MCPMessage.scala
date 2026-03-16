package spice.mcp

import fabric.*
import fabric.rw.*

case class JsonRPCRequest(jsonrpc: String, id: Option[Json], method: String, params: Option[Json])

object JsonRPCRequest {
  given rw: RW[JsonRPCRequest] = RW.gen
}

case class JsonRPCResponse(jsonrpc: String, id: Json, result: Option[Json] = None, error: Option[JsonRPCError] = None)

object JsonRPCResponse {
  given rw: RW[JsonRPCResponse] = RW.gen

  def success(id: Json, result: Json): JsonRPCResponse =
    JsonRPCResponse(jsonrpc = "2.0", id = id, result = Some(result))

  def error(id: Json, error: JsonRPCError): JsonRPCResponse =
    JsonRPCResponse(jsonrpc = "2.0", id = id, error = Some(error))
}

case class JsonRPCError(code: Int, message: String, data: Option[Json] = None)

object JsonRPCError {
  given rw: RW[JsonRPCError] = RW.gen

  val ParseError: JsonRPCError = JsonRPCError(-32700, "Parse error")
  val InvalidRequest: JsonRPCError = JsonRPCError(-32600, "Invalid Request")
  val MethodNotFound: JsonRPCError = JsonRPCError(-32601, "Method not found")
  val InvalidParams: JsonRPCError = JsonRPCError(-32602, "Invalid params")
  val InternalError: JsonRPCError = JsonRPCError(-32603, "Internal error")
}

case class JsonRPCNotification(jsonrpc: String, method: String, params: Option[Json] = None)

object JsonRPCNotification {
  given rw: RW[JsonRPCNotification] = RW.gen
}
