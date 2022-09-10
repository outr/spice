name := "spice"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"

val scala213: String = "2.13.8"
val scala3: String = "3.2.0"

scalaVersion := scala213

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq(scala213, scala3)

def dep: Dependencies.type = Dependencies

lazy val root = project.in(file("."))
	.aggregate(
		coreJS, coreJVM, clientJS, clientJVM, clientImplementationOkHttp
	)
	.settings(
		publish := {},
		publishLocal := {}
	)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
	.settings(
		name := "youi-core",
		description := "Core functionality leveraged and shared by most other sub-projects of YouI.",
		libraryDependencies ++= Seq(
			dep.profig, dep.scribe, dep.fabricParse, dep.reactify, dep.catsEffect, dep.fs2, dep.literally, dep.moduload,
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
		name := "youi-client",
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
		libraryDependencies ++= Seq(
			dep.okHttp
		)
	)