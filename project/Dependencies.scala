import sbt.*

object Dependencies {
  object version {
    val profig: String = "3.4.17"
    
    val scribe: String = "3.15.3"

    val fabric: String = "1.15.5"
    
    val reactify: String = "4.1.3"

    val rapid: String = "0.3.2"
    
    val literally: String = "1.2.0"
    
    val okHttp: String = "4.12.0"

    val httpMime: String = "4.5.14"
    
    val undertow: String = "2.3.18.Final"
    
    val moduload: String = "1.1.7"

    val scalaJSDOM: String = "2.8.0"

    val scalaTest: String = "3.2.19"
  }

  val profig: ModuleID = "com.outr" %% "profig" % version.profig
	val scribe: ModuleID = "com.outr" %% "scribe" % version.scribe
	val scribeRapid: ModuleID = "com.outr" %% "scribe-rapid" % version.scribe
  val scribeSlf4j: ModuleID = "com.outr" %% "scribe-slf4j" % version.scribe
	val fabricParse: ModuleID = "org.typelevel" %% "fabric-io" % version.fabric
	val reactify: ModuleID = "com.outr" %% "reactify" % version.reactify
  val rapid: ModuleID = "com.outr" %% "rapid-core" % version.rapid
	val literally: ModuleID = "org.typelevel" %% "literally" % version.literally
	val okHttp: ModuleID = "com.squareup.okhttp3" % "okhttp" % version.okHttp
  val httpMime: ModuleID = "org.apache.httpcomponents" % "httpmime" % version.httpMime
	val undertow: ModuleID = "io.undertow" % "undertow-core" % version.undertow
	val moduload: ModuleID = "com.outr" %% "moduload" % version.moduload

	val scalaTest: ModuleID = "org.scalatest" %% "scalatest" % version.scalaTest % Test
}