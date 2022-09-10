package spice.http.client

object HttpClientImplementationManager {
  private var creator: Option[HttpClientConfig => HttpClientImplementation] = None

  def apply(config: HttpClientConfig): HttpClientImplementation = creator
    .getOrElse(throw new RuntimeException(s"No HttpClientImplementationManager defined. Moduload.load() must be run first."))
    .apply(config)

  def register(creator: HttpClientConfig => HttpClientImplementation): Unit = synchronized {
    if (this.creator.nonEmpty) throw new RuntimeException(s"HttpClientImplementation already defined!")
    this.creator = Some(creator)
  }
}
