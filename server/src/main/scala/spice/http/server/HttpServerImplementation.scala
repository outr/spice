package spice.http.server

import rapid.Task

trait HttpServerImplementation {
  def isRunning: Boolean

  def start(server: HttpServer): Task[Unit]

  def stop(server: HttpServer): Task[Unit]

  /** Re-read every HTTPS listener's keystore from disk and swap the running SSLContext's
    * key material atomically — no listener teardown, no dropped connections. New TLS
    * handshakes after this call use the new cert; in-flight TLS sessions continue with
    * their handshake-derived keys until they close.
    *
    * Default falls back to `stop(); start()` for impls that haven't wired hot-reload.
    * Override when the underlying server supports a real swap (e.g. Undertow). */
  def reloadCertificates(server: HttpServer): Task[Unit] =
    stop(server).flatMap(_ => start(server))
}
