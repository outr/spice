package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.server.acme.dns.{DnsProvider, ManualDnsProvider}
import spice.http.server.acme.{AcmeChallenge, AcmeConfig}

import java.io.File

class AcmeDns01Spec extends AnyWordSpec with Matchers {
  private val tmp: File = java.nio.file.Files.createTempDirectory("acme-dns-spec").toFile

  "AcmeConfig" should {
    "reject wildcard domains under HTTP-01" in {
      val ex = intercept[IllegalArgumentException] {
        AcmeConfig(
          contactEmail = "ops@example.com",
          domains      = List("example.com", "*.example.com"),
          storageDir   = tmp,
          challenge    = AcmeChallenge.Http01
        )
      }
      ex.getMessage should include("Wildcard")
      ex.getMessage should include("*.example.com")
    }

    "accept wildcard domains under DNS-01" in {
      val config = AcmeConfig(
        contactEmail = "ops@example.com",
        domains      = List("example.com", "*.example.com"),
        storageDir   = tmp,
        challenge    = AcmeChallenge.Dns01(ManualDnsProvider())
      )
      config.domains should contain("*.example.com")
      config.primaryDomain shouldBe "example.com"
    }
  }

  "DnsProvider.splitApex" should {
    "split a typical _acme-challenge name" in {
      DnsProvider.splitApex("_acme-challenge.example.com") shouldBe ("example.com", "_acme-challenge")
    }
    "split a deeper subdomain" in {
      DnsProvider.splitApex("_acme-challenge.foo.bar.example.com") shouldBe ("example.com", "_acme-challenge.foo.bar")
    }
    "leave a bare apex alone" in {
      DnsProvider.splitApex("example.com") shouldBe ("", "example.com")
    }
    "tolerate a trailing dot" in {
      DnsProvider.splitApex("_acme-challenge.example.com.") shouldBe ("example.com", "_acme-challenge")
    }
  }

  "ManualDnsProvider" should {
    "call prompt on publish and unpublish, await on publish" in {
      val prompts = scala.collection.mutable.ListBuffer.empty[String]
      val awaited = new java.util.concurrent.atomic.AtomicInteger(0)
      val provider = ManualDnsProvider(
        prompt       = msg => prompts += msg,
        awaitConfirm = () => awaited.incrementAndGet()
      )
      provider.publish("_acme-challenge.example.com", "abc.def").sync()
      provider.unpublish("_acme-challenge.example.com", "abc.def").sync()

      // publish prompts and waits; unpublish only prompts (default confirmRemoval=false)
      prompts should have size 2
      prompts.head should include("_acme-challenge.example.com")
      prompts.head should include("abc.def")
      prompts.head should include("TXT")
      awaited.get() shouldBe 1
    }

    "wait on unpublish too when confirmRemoval=true" in {
      val awaited = new java.util.concurrent.atomic.AtomicInteger(0)
      val provider = ManualDnsProvider(
        prompt         = _ => (),
        awaitConfirm   = () => awaited.incrementAndGet(),
        confirmRemoval = true
      )
      provider.publish("_acme-challenge.example.com", "x").sync()
      provider.unpublish("_acme-challenge.example.com", "x").sync()
      awaited.get() shouldBe 2
    }
  }
}
