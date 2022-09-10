name := "spice"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"

val scala213: String = "2.13.8"
val scala3: String = "3.2.0"

scalaVersion := scala213

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq(scala213, scala3)

libraryDependencies ++= Seq(
	"com.outr" %% "profig" % "3.4.1",
	"com.outr" %% "scribe" % "3.10.3",
	"com.outr" %% "fabric-parse" % "1.3.0",
	"com.outr" %% "reactify" % "4.0.8",
	"org.typelevel" %% "cats-effect" % "3.3.14",
	"co.fs2" %% "fs2-core" % "3.2.12",
	"org.typelevel" %% "literally" % "1.1.0",
	"com.squareup.okhttp3" % "okhttp" % "4.10.0",
	"io.undertow" % "undertow-core" % "2.2.19.Final",
	"com.outr" %% "moduload" % "1.1.6",
	"org.scalatest" %% "scalatest" % "3.2.13" % Test
)