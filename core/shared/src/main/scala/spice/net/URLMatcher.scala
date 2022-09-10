package spice.net

trait URLMatcher {
  def matches(url: URL): Boolean
}