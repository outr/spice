name := "spice"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"

val scala213: String = "2.13.8"
val scala3: String = "3.1.3"

scalaVersion := scala213

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq(scala213, scala3)

libraryDependencies ++= Seq(
	"com.outr" %% "profig" % "3.4.0",
	"com.outr" %% "scribe" % "3.10.2",
	"com.outr" %% "fabric-parse" % "1.3.0",
	"com.outr" %% "reactify" % "4.0.8",
	"org.typelevel" %% "cats-effect" % "3.3.13",
	"co.fs2" %% "fs2-core" % "3.2.8",
	"org.typelevel" %% "literally" % "1.1.0",
	"io.undertow" % "undertow-core" % "2.2.19.Final",
	"com.outr" %% "moduload" % "1.1.5",
	"org.scalatest" %% "scalatest" % "3.2.13" % Test
)
