package spice.http.durable

/** Carries a stable `code` alongside a human-readable message across an RPC `response-error` frame,
  * so the caller can branch on the failure kind rather than parsing the message. */
class RpcException(val code: String, message: String) extends RuntimeException(message)
