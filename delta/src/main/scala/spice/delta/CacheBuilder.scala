package spice.delta

trait CacheBuilder {
  def isStale: Boolean
  def buildCache(): CachedInformation
}
