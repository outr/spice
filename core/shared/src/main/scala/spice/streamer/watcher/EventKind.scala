package spice.streamer.watcher

sealed trait EventKind

object EventKind {
  case object Create extends EventKind
  case object Modify extends EventKind
  case object Delete extends EventKind
}