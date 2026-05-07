package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.server.acme.AcmeKeyStore

import java.io.File
import java.nio.file.Files
import java.time.Instant
import scala.concurrent.duration.*

class AcmeKeyStoreSpec extends AnyWordSpec with Matchers {
  private def tempDir(): File = Files.createTempDirectory("spice-acme-test").toFile

  "AcmeKeyStore.loadOrCreateKeyPair" should {
    "create a fresh PEM-encoded key pair when the file is missing" in {
      val dir = tempDir()
      val keyFile = new File(dir, "k.pem")
      keyFile.exists() shouldBe false
      val pair = AcmeKeyStore.loadOrCreateKeyPair(keyFile)
      pair.getPrivate should not be null
      pair.getPublic should not be null
      keyFile.exists() shouldBe true
      keyFile.length() should be > 0L
    }

    "reload the same key pair on subsequent calls" in {
      val dir = tempDir()
      val keyFile = new File(dir, "k.pem")
      val first = AcmeKeyStore.loadOrCreateKeyPair(keyFile)
      val second = AcmeKeyStore.loadOrCreateKeyPair(keyFile)
      // KeyPair equality isn't defined, but the encoded bytes must match.
      first.getPrivate.getEncoded should contain theSameElementsInOrderAs second.getPrivate.getEncoded
      first.getPublic.getEncoded should contain theSameElementsInOrderAs second.getPublic.getEncoded
    }

    "create the parent directory if missing" in {
      val dir = tempDir()
      val nested = new File(dir, "nested/deeper")
      val keyFile = new File(nested, "k.pem")
      AcmeKeyStore.loadOrCreateKeyPair(keyFile)
      keyFile.exists() shouldBe true
    }
  }

  "AcmeKeyStore.certNotAfter" should {
    "return None when the keystore file does not exist" in {
      val dir = tempDir()
      val ks = new File(dir, "missing.jks")
      AcmeKeyStore.certNotAfter(ks, "password", "example.com") shouldBe None
    }
  }

  "AcmeKeyStore.needsRenewal" should {
    "be true when the cert is already expired" in {
      val past = Instant.now().minusSeconds(60)
      AcmeKeyStore.needsRenewal(past, threshold = 30.days) shouldBe true
    }

    "be true when the cert expires within the threshold" in {
      val soon = Instant.now().plusSeconds(60)
      AcmeKeyStore.needsRenewal(soon, threshold = 30.days) shouldBe true
    }

    "be false when the cert expires beyond the threshold" in {
      val later = Instant.now().plusSeconds(60.days.toSeconds)
      AcmeKeyStore.needsRenewal(later, threshold = 30.days) shouldBe false
    }

    "treat expiry exactly at the threshold cutoff as needing renewal" in {
      // The cutoff comparison is `notAfter.isAfter(cutoff)`, so equality fails the check
      // and triggers renewal. Documenting this behavior so downstream callers can rely on
      // it (renew sooner is always safe).
      val now = Instant.parse("2026-01-01T00:00:00Z")
      val cutoff = now.plusMillis(30.days.toMillis)
      AcmeKeyStore.needsRenewal(cutoff, threshold = 30.days, now = now) shouldBe true
    }
  }
}
