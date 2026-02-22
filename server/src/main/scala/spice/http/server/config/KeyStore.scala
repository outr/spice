package spice.http.server.config

import fabric.rw.*

import java.io.File

object KeyStore {
  given rw: RW[KeyStore] = RW.gen
}

case class KeyStore(path: String = "keystore.jks", password: String = "password") {
  lazy val location: File = new File(path)
}