ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "TinyAssignment",
    idePackagePrefix := Some("com.tiny")
  )

val AkkaVersion = "2.7.0"
val AkkaHttpVersion = "10.5.0"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion
)
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5" % Runtime
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test
libraryDependencies += "org.scalamock" %% "scalamock" % "5.2.0" % Test
