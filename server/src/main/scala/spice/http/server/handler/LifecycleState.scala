package spice.http.server.handler

object LifecycleState {
  case object Pre extends LifecycleState

  case object Handler extends LifecycleState

  case object Post extends LifecycleState
}

sealed trait LifecycleState