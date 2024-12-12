package spice.ajax

import org.scalajs.dom.XMLHttpRequest
import rapid.Task
import reactify._

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

  // TODO: dequeue if not already running
  def cancel(): Unit = request.cancel()     // TODO: does cancel fire onComplete with a failure?
}