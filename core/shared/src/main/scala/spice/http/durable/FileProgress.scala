package spice.http.durable

/** Progress pulse for an in-flight transfer (either direction). */
case class FileProgress(transferId: String, bytesSoFar: Long, totalBytes: Long) {
  /** Fraction complete in `[0, 1]`, or `0` when the total is unknown. */
  def fraction: Double = if (totalBytes <= 0L) 0.0 else bytesSoFar.toDouble / totalBytes.toDouble
}
