package spice.net

import fabric.rw._
import spice.UserException

class EmailAddress private(val value: String) {
  private lazy val index = value.indexOf('@')
  lazy val local: String = value.substring(0, index)
  lazy val domain: String = value.substring(index + 1)

  def normalize(): EmailAddress = EmailAddress(value.toLowerCase)
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
    EmailAddress(s"$l@$d").normalize()
  }

  override def toString: String = value

  override def hashCode(): Int = value.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case that: EmailAddress => this.value == that.value
    case _ => false
  }
}

object EmailAddress {
  implicit val rw: RW[EmailAddress] = RW.string(
    asString = _.value,
    fromString = new EmailAddress(_)
  )

  private val EmailRegex = """(.+)@(.+)[.](.+)""".r

  def parse(email: String): Option[EmailAddress] = parse(email, warn = true)

  def parse(email: String, warn: Boolean): Option[EmailAddress] = email.trim match {
    case null | "" => None
    case EmailRegex(local, domain, tld) => Some(new EmailAddress(s"$local@$domain.$tld"))
    case _ =>
      if (warn) scribe.warn(s"Unrecognized email address: [$email]")
      None
  }

  def apply(email: String): EmailAddress = parse(email)
    .getOrElse(throw UserException(s"Invalid email address: $email"))

  def unsafe(email: String): EmailAddress = new EmailAddress(email)
}