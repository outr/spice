name := "spice"
ThisBuild / organization := "com.outr"
ThisBuild / version := "0.0.1"

val scala213: String = "2.13.8"
val scala3: String = "3.2.0"

ThisBuild / scalaVersion := scala213
ThisBuild / scalacOptions ++= Seq("-deprecation")
ThisBuild / crossScalaVersions := Seq(scala213, scala3)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeProfileName := "com.outr"
//ThisBuild / publishMavenStyle := true
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

def dep: Dependencies.type = Dependencies

lazy val root = project.in(file("."))
	.aggregate(
		coreJS, coreJVM,
		clientJS, clientJVM, clientImplementationOkHttp,
		server, serverImplementationUndertow
	)
	.settings(
		publish := {},
		publishLocal := {}
	)

lazy val core = crossProject(JSPlatform, JVMPlatform)
	.in(file("core"))
	.settings(
		name := "spice-core",
		description := "Core functionality leveraged and shared by most other sub-projects of YouI.",
		libraryDependencies ++= Seq(
			dep.profig, dep.scribe, dep.scribeCats, dep.fabricParse, dep.reactify, dep.catsEffect, dep.fs2, dep.literally,
			dep.moduload,
			dep.scalaTest, dep.catsEffectTesting
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
			dep.scalaTest, dep.catsEffectTesting
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
			dep.scalaTest, dep.catsEffectTesting
		)
	)

lazy val server = project
	.dependsOn(coreJVM)
	.in(file("server"))
	.settings(
		name := "spice-server",
		libraryDependencies ++= Seq(
			dep.scalaTest, dep.catsEffectTesting
		)
	)

lazy val serverImplementationUndertow = project
	.dependsOn(server)
	.in(file("server/implementation/undertow"))
	.settings(
		name := "spice-server-undertow",
		libraryDependencies ++= Seq(
			dep.undertow, dep.scribeSlf4j,
			dep.scalaTest, dep.catsEffectTesting
		)
	)