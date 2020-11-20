package spice.http

import spice.net.{IP, URL}

case class HttpRequest(method: HttpMethod = HttpMethod.Get,
                       source: IP = IP.LocalHost,
                       url: URL = URL())
