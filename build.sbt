name := "spice"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.13.3"

crossScalaVersions := Seq("2.13.3", "2.12.12", "2.11.12", "3.0.0-M1")

libraryDependencies ++= Seq(
	"com.outr" %% "profig" % "3.0.4",
	"com.outr" %% "scribe" % "3.0.4",
	"io.undertow" % "undertow-core" % "2.1.3.Final",
	"com.outr" %% "moduload" % "1.0.1"
)