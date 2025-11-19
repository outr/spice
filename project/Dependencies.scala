import sbt.*

object Dependencies {
  object version {
    val profig: String = "3.4.18"
    
    val scribe: String = "3.17.0"

    val fabric: String = "1.18.4"
    
    val reactify: String = "4.1.5"

    val rapid: String = "2.3.1"
    
    val literally: String = "1.2.0"
    
    val okHttp: String = "5.3.2"

    val netty: String = "4.2.7.Final"

    val httpMime: String = "4.5.14"
    
    val undertow: String = "2.3.20.Final"
    
    val moduload: String = "1.1.7"

    val scalaJSDOM: String = "2.8.1"

    val scalaTest: String = "3.2.19"
  }

  val profig: ModuleID = "com.outr" %% "profig" % version.profig
	val scribe: ModuleID = "com.outr" %% "scribe" % version.scribe
  val scribeSlf4j: ModuleID = "com.outr" %% "scribe-slf4j" % version.scribe
	val fabricParse: ModuleID = "org.typelevel" %% "fabric-io" % version.fabric
	val reactify: ModuleID = "com.outr" %% "reactify" % version.reactify
  val rapid: ModuleID = "com.outr" %% "rapid-core" % version.rapid
  val rapidScribe: ModuleID = "com.outr" %% "rapid-scribe" % version.rapid
	val literally: ModuleID = "org.typelevel" %% "literally" % version.literally
	val okHttp: ModuleID = "com.squareup.okhttp3" % "okhttp-jvm" % version.okHttp
  val nettyCodecHttp: ModuleID = "io.netty" % "netty-codec-http" % version.netty
  val nettyHandler: ModuleID = "io.netty" % "netty-handler" % version.netty
  val nettyHandlerProxy: ModuleID = "io.netty" % "netty-handler-proxy" % version.netty
  val nettyTransport: ModuleID = "io.netty" % "netty-transport" % version.netty
  val nettyBuffer: ModuleID = "io.netty" % "netty-buffer" % version.netty
  val nettyCommon: ModuleID = "io.netty" % "netty-common" % version.netty
  val nettyResolver: ModuleID = "io.netty" % "netty-resolver" % version.netty
  val httpMime: ModuleID = "org.apache.httpcomponents" % "httpmime" % version.httpMime
	val undertow: ModuleID = "io.undertow" % "undertow-core" % version.undertow
	val moduload: ModuleID = "com.outr" %% "moduload" % version.moduload

	val scalaTest: ModuleID = "org.scalatest" %% "scalatest" % version.scalaTest % Test
}