package spice.http.server.acme.dns

import rapid.Task

/** A `DnsProvider` for the case where there's no API integration: print the record to
  * stdout, then block until the user signals (via stdin) that they've added it to their
  * DNS console.
  *
  * Useful for one-off cert issuance, environments without programmatic DNS access, and
  * testing the rest of the ACME flow without committing to a specific provider. Not
  * suitable for unattended renewal — use a real provider for that.
  *
  * `prompt` and `awaitConfirm` are pluggable so tests can drive the flow without an
  * actual TTY. The defaults read from stdin / write to stdout. */
case class ManualDnsProvider(prompt: String => Unit = ManualDnsProvider.defaultPrompt,
                             awaitConfirm: () => Unit = ManualDnsProvider.defaultAwait,
                             confirmRemoval: Boolean = false) extends DnsProvider {
  override def publish(recordName: String, value: String): Task[Unit] = Task {
    val msg =
      s"""
         |==========================================================================
         |ACME DNS-01 challenge — please add the following TXT record:
         |
         |  Name:  $recordName
         |  Type:  TXT
         |  Value: $value
         |  TTL:   60 (or shortest supported)
         |
         |After the record is published AND visible from public resolvers
         |(verify with: dig +short TXT $recordName), press ENTER to continue.
         |==========================================================================
         |""".stripMargin
    prompt(msg)
    awaitConfirm()
  }

  override def unpublish(recordName: String, value: String): Task[Unit] = Task {
    if (confirmRemoval) {
      prompt(
        s"""
           |==========================================================================
           |ACME DNS-01 challenge complete — you may now REMOVE the TXT record:
           |
           |  Name:  $recordName
           |  Value: $value
           |
           |Press ENTER to acknowledge.
           |==========================================================================
           |""".stripMargin
      )
      awaitConfirm()
    } else {
      prompt(s"ACME DNS-01: you may now remove TXT record $recordName (value: $value).\n")
    }
  }
}

object ManualDnsProvider {
  private val defaultPrompt: String => Unit = msg => System.out.print(msg)
  private val defaultAwait: () => Unit      = () => { val _ = scala.io.StdIn.readLine() }
}
