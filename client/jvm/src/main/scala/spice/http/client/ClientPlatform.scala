package spice.http.client

object ClientPlatform {
  def defaultSaveDirectory: String = System.getProperty("java.io.tmpdir")
}