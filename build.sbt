name := "spice"
ThisBuild / organization := "com.outr"
ThisBuild / version := "0.10.13"

val scala213: String = "2.13.17"

val scala3: String = "3.3.7"

ThisBuild / scalaVersion := scala3
ThisBuild / scalacOptions ++= Seq("-deprecation")
ThisBuild / javacOptions ++= Seq("-source", "11", "-target", "11")
ThisBuild / crossScalaVersions := Seq(scala213, scala3)

publishMavenStyle := true

ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeProfileName := "com.outr"
ThisBuild / licenses := Seq("MIT" -> url("https://github.com/outr/spice/blob/master/LICENSE"))
ThisBuild / sonatypeProjectHosting := Some(xerial.sbt.Sonatype.GitHubHosting("outr", "spice", "matt@matthicks.com"))
ThisBuild / homepage := Some(url("https://github.com/outr/spice"))
ThisBuild / scmInfo := Some(
	ScmInfo(
		url("https://github.com/outr/spice"),
		"scm:git@github.com:outr/spice.git"
	)
)
ThisBuild / developers := List(
	Developer(id="darkfrog", name="Matt Hicks", email="matt@matthicks.com", url=url("https://matthicks.com"))
)

ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / outputStrategy := Some(StdoutOutput)
ThisBuild / Test / testOptions += Tests.Argument("-oDF")

def groupTests(tests: Seq[TestDefinition]): Seq[Tests.Group] = tests.map { test =>
	Tests.Group(
		name = test.name,
		tests = Seq(test),
		runPolicy = Tests.SubProcess(ForkOptions())
	)
}

def dep: Dependencies.type = Dependencies

lazy val root = project.in(file("."))
	.aggregate(
		coreJS, coreJVM,
		clientJS, clientJVM, clientImplementationOkHttp, clientImplementationJVM, clientImplementationNetty,
		delta,
		server, serverImplementationUndertow,
		openAPI
	)
	.settings(
		publish := {},
		publishLocal := {}
	)

lazy val core = crossProject(JSPlatform, JVMPlatform)
	.in(file("core"))
	.settings(
		name := "spice-core",
		description := "Core functionality leveraged and shared by most other sub-projects of Spice.",
		libraryDependencies ++= Seq(
			dep.profig, dep.scribe, dep.fabricParse, dep.reactify, dep.rapid, dep.rapidScribe,
			dep.literally, dep.moduload,
			dep.scalaTest
		)
	)
	.jsSettings(
		libraryDependencies ++= Seq(
			"org.scala-js" %%% "scalajs-dom" % dep.version.scalaJSDOM
		)
	)

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val client = crossProject(JSPlatform, JVMPlatform)
	.in(file("client"))
	.dependsOn(core)
	.settings(
		name := "spice-client",
		libraryDependencies ++= Seq(
			dep.scalaTest
		)
	)

lazy val clientJS = client.js
lazy val clientJVM = client.jvm

lazy val clientImplementationOkHttp = project
	.dependsOn(clientJVM)
	.in(file("client/implementation/okhttp"))
	.settings(
		name := "spice-client-okhttp",
		libraryDependencies ++= Seq(
			dep.okHttp,
			dep.scalaTest
		)
	)

lazy val clientImplementationJVM = project
	.dependsOn(clientJVM)
	.in(file("client/implementation/jvm"))
	.settings(
		name := "spice-client-jvm",
		libraryDependencies ++= Seq(
			dep.httpMime,
			dep.scalaTest
		)
	)

lazy val clientImplementationNetty = project
	.dependsOn(clientJVM)
	.in(file("client/implementation/netty"))
	.settings(
		name := "spice-client-netty",
		libraryDependencies ++= Seq(
			dep.nettyCodecHttp,
			dep.nettyHandler,
			dep.nettyHandlerProxy,
			dep.nettyTransport,
			dep.nettyBuffer,
			dep.nettyCommon,
			dep.nettyResolver,
			dep.scalaTest
		)
	)


lazy val delta = project
	.dependsOn(coreJVM)
	.in(file("delta"))
	.settings(
		name := "spice-delta",
		libraryDependencies ++= Seq(
			dep.scalaTest
		)
	)

lazy val server = project
	.dependsOn(coreJVM, delta, clientImplementationJVM % "test->test")
	.in(file("server"))
	.settings(
		name := "spice-server",
		Test / testGrouping := groupTests((Test / definedTests).value),
		libraryDependencies ++= Seq(
			dep.scalaTest
		)
	)

lazy val serverImplementationUndertow = project
	.dependsOn(
		server,
		clientImplementationJVM % "test->test"
	)
	.in(file("server/implementation/undertow"))
	.settings(
		name := "spice-server-undertow",
		fork := true,
		libraryDependencies ++= Seq(
			dep.undertow, dep.scribeSlf4j,
			dep.scalaTest
		)
	)

lazy val openAPI = project
	.dependsOn(server, server % "test->test")
	.in(file("openapi"))
	.settings(
		name := "spice-openapi",
		fork := true,
		libraryDependencies ++= Seq(
			dep.scalaTest
		)
	)