package spice.api.server

case class WsParamDescriptor(name: String, typeName: String, optional: Boolean)

case class WsMethodDescriptor(name: String, params: List[WsParamDescriptor])
