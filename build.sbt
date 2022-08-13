name := "spice"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"
scalaVersion := "3.1.3"

scalacOptions ++= Seq("-deprecation")

//crossScalaVersions := Seq("2.13.3", "2.12.12", "2.11.12", "3.0.0-M1")

libraryDependencies ++= Seq(
	"com.outr" %% "profig" % "3.4.0",
	"com.outr" %% "scribe" % "3.10.2",
	"com.outr" %% "fabric-parse" % "1.3.0",
	"org.typelevel" %% "literally" % "1.1.0",
	"io.undertow" % "undertow-core" % "2.2.19.Final",
	"com.outr" %% "moduload" % "1.1.5",
	"org.scalatest" %% "scalatest" % "3.2.13" % Test
)
