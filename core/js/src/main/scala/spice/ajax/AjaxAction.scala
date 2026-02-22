package spice.ajax

import org.scalajs.dom.XMLHttpRequest
import rapid.Task
import reactify.*

import scala.util.Try

class AjaxAction(request: AjaxRequest) {
  lazy val task: Task[Try[XMLHttpRequest]] = request.completable
  private[ajax] val _state = Var[ActionState](ActionState.New)
  def state: Val[ActionState] = _state
  def loaded: Val[Double] = request.loaded
  def total: Val[Double] = request.total
  def percentage: Val[Int] = request.percentage
  def cancelled: Val[Boolean] = request.cancelled

  private[ajax] def start(manager: AjaxManager): Unit = {
    if (!cancelled()) {
      _state @= ActionState.Running
      task.map { _ =>
        _state @= ActionState.Finished
        manager.remove(this)
      }.start()
      request.send()
    } else {
      manager.remove(this)
    }
  }

  /**
   * Cancels this action. If the action is still enqueued (not yet running),
   * it will be marked as cancelled so it won't start. If already running,
   * the underlying XHR abort is called. Note: XHR abort triggers the
   * onreadystatechange handler with readyState=4 and status=0, which
   * results in the completable receiving a Failure.
   */
  def cancel(): Unit = {
    request.cancel()
    if (state() == ActionState.Enqueued) {
      _state @= ActionState.Finished
    }
  }
}