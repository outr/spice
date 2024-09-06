import sbt.*

object Dependencies {
  object version {
    val profig: String = "3.4.15"
    
    val scribe: String = "3.15.0"

    val fabric: String = "1.15.3"
    
    val reactify: String = "4.1.2"
    
    val cats: String = "3.5.4"
    
    val fs2: String = "3.11.0"
    
    val literally: String = "1.2.0"
    
    val okHttp: String = "4.12.0"

    val httpMime: String = "4.5.14"
    
    val undertow: String = "2.3.17.Final"
    
    val moduload: String = "1.1.7"

    val scalaJSDOM: String = "2.8.0"

    val scalaTest: String = "3.2.19"
    
    val catsEffectTesting: String = "1.5.0"
  }

  val profig: ModuleID = "com.outr" %% "profig" % version.profig
	val scribe: ModuleID = "com.outr" %% "scribe" % version.scribe
  val scribeSlf4j: ModuleID = "com.outr" %% "scribe-slf4j" % version.scribe
  val scribeCats: ModuleID = "com.outr" %% "scribe-cats" % version.scribe
	val fabricParse: ModuleID = "org.typelevel" %% "fabric-io" % version.fabric
	val reactify: ModuleID = "com.outr" %% "reactify" % version.reactify
	val catsEffect: ModuleID = "org.typelevel" %% "cats-effect" % version.cats
	val fs2: ModuleID = "co.fs2" %% "fs2-core" % version.fs2
  val fs2IO: ModuleID = "co.fs2" %% "fs2-io" % version.fs2
	val literally: ModuleID = "org.typelevel" %% "literally" % version.literally
	val okHttp: ModuleID = "com.squareup.okhttp3" % "okhttp" % version.okHttp
  val httpMime: ModuleID = "org.apache.httpcomponents" % "httpmime" % version.httpMime
	val undertow: ModuleID = "io.undertow" % "undertow-core" % version.undertow
	val moduload: ModuleID = "com.outr" %% "moduload" % version.moduload

	val scalaTest: ModuleID = "org.scalatest" %% "scalatest" % version.scalaTest % Test
  val catsEffectTesting: ModuleID = "org.typelevel" %% "cats-effect-testing-scalatest" % version.catsEffectTesting % Test
}