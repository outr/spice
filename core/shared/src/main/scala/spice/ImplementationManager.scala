package spice

import moduload.Moduload

trait ImplementationManager[Implementation, Config] {
  private var creator: Option[Config => Implementation] = None

  Moduload.load()

  def apply(config: Config): Implementation = creator
    .getOrElse(throw new NoImplementationException)
    .apply(config)

  def register(creator: Config => Implementation): Unit = synchronized {
    require(this.creator.isEmpty, "An implementation manager is already registered!")
    this.creator = Some(creator)
  }
}