package spice.mcp

import rapid.Task

import java.util.concurrent.ConcurrentHashMap

/**
 * Pluggable storage for MCP sessions. Mirrors the precedent set by
 * [[spice.mcp.oauth.OAuthStore]]: the default is in-memory (fine for
 * single-process servers that don't care about cross-restart durability),
 * but applications can override [[MCPServer.sessionStore]] with a
 * persistent implementation so sessions survive server restarts.
 *
 * Without persistence, every restart bricks every connected MCP client
 * (their cached `Mcp-Session-Id` no longer matches anything server-side,
 * the next call returns "Invalid or missing Mcp-Session-Id", and clients
 * like Claude Code surface that as "session expired" without auto-
 * reinitializing). With persistence, sessions outlive the JVM and clients
 * reconnect transparently.
 *
 * The store is operated through `rapid.Task` so implementations can talk
 * to a database, a network cache, or anything else that fits.
 */
trait SessionStore {
  /** Look up a session by its id. Returns `None` for missing/unknown ids. */
  def get(sessionId: String): Task[Option[MCPSession]]

  /** Persist a session (or replace an existing one with the same id). */
  def put(session: MCPSession): Task[Unit]

  /** Remove a session by id. Safe to call on a non-existent id. */
  def remove(sessionId: String): Task[Unit]
}

/**
 * Default `SessionStore` — a thread-safe in-memory map. Suitable for single-
 * process development setups. Sessions are lost on restart; override
 * [[MCPServer.sessionStore]] with a persistent implementation for production.
 */
class InMemorySessionStore extends SessionStore {
  private val sessions = new ConcurrentHashMap[String, MCPSession]()

  override def get(sessionId: String): Task[Option[MCPSession]] =
    Task.pure(Option(sessions.get(sessionId)))

  override def put(session: MCPSession): Task[Unit] =
    Task { sessions.put(session.id, session); () }

  override def remove(sessionId: String): Task[Unit] =
    Task { sessions.remove(sessionId); () }
}
