package spice.http.client

import spice.net.{DNS, IP}

import java.net.{InetAddress, UnknownHostException}
import scala.jdk.CollectionConverters.*

/** Preserve Android's resolver ordering, IPv6 and alternate routes. Custom resolvers still own their overrides. */
private[client] object OkHttpDNS {
  def apply(dns: DNS): okhttp3.Dns =
    if (dns eq DNS.default) okhttp3.Dns.SYSTEM
    else (hostname: String) => {
      val addresses = dns.lookupAll(hostname).sync().map {
        case ip: IP.v6 if ip.scope.nonEmpty => InetAddress.getByName(ip.addressString)
        case ip => InetAddress.getByAddress(ip.bytes)
      }
      if (addresses.isEmpty) throw new UnknownHostException(hostname)
      addresses.asJava
    }
}
