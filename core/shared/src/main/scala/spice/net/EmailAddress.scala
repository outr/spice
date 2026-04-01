package spice.net

import fabric.rw.*
import spice.UserException

class EmailAddress private(val value: String, val name: Option[String] = None) {
  private lazy val index: Int = {
    val i = value.indexOf('@')
    if (i == -1) throw UserException(s"Invalid email address (missing @): $value")
    i
  }
  lazy val local: String = value.substring(0, index)
  lazy val domain: String = value.substring(index + 1)

  lazy val complete: String = name match {
    case Some(n) => s"$n <$value>"
    case None => value
  }

  def withName(name: String): EmailAddress = new EmailAddress(value, Some(name))

  def normalize(): EmailAddress = new EmailAddress(value.toLowerCase, name)
  def canonical(excludePlus: Boolean = false): EmailAddress = {
    var l = local
      .replaceAll("[(].*?[)]", "")
      .replaceAll("[{].*?[}]", "")
    if (excludePlus) {
      val index = l.indexOf('+')
      if (index != -1) {
        l = l.substring(0, index)
      }
    }
    val d = domain
      .replaceAll("[(].*?[)]", "")
      .replaceAll("[{].*?[}]", "")
    new EmailAddress(s"$l@$d", name).normalize()
  }

  override def toString: String = value

  override def hashCode(): Int = value.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case that: EmailAddress => this.value == that.value
    case _ => false
  }
}

object EmailAddress {
  given rw: RW[EmailAddress] = RW.string(
    asString = _.complete,
    fromString = s => parse(s, warn = false).getOrElse(new EmailAddress(s))
  )

  private val EmailRegex = """(.+)@(.+)[.](.+)""".r
  private val NamedEmailRegex = """(.+?)\s*<(.+)>""".r

  def parse(email: String): Option[EmailAddress] = parse(email, warn = true)

  def parse(email: String, warn: Boolean): Option[EmailAddress] = Option(email).map(_.trim).filterNot(_.isEmpty).flatMap {
    case NamedEmailRegex(name, address) =>
      parseAddress(address.trim, warn).map(_.withName(name.trim))
    case address =>
      parseAddress(address, warn)
  }

  private def parseAddress(address: String, warn: Boolean): Option[EmailAddress] = address match {
    case EmailRegex(local, domain, tld) => Some(new EmailAddress(s"$local@$domain.$tld"))
    case _ =>
      if (warn) scribe.warn(s"Unrecognized email address: [$address]")
      None
  }

  def apply(email: String): EmailAddress = parse(email)
    .getOrElse(throw UserException(s"Invalid email address: $email"))

  def unsafe(email: String): EmailAddress = new EmailAddress(email)
}
