package spice.maintenance

trait MaintenanceTaskInstance {
  def name: String

  def status: TaskStatus

  def cancel(): Unit
}
