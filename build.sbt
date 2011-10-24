name := "aiclass"

version := "1.0"

organization := "dk.rmstjerne"

scalaVersion := "2.9.1"

resolvers ++= Seq(
   "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
   "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
)

libraryDependencies ++= Seq(
    "junit" % "junit" % "4.8" % "test",
    "org.scalatest" %% "scalatest" % "1.6.1" % "test",
    "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test",
    "se.scalablesolutions.akka" % "akka-actor" % "1.2",
    "se.scalablesolutions.akka" % "akka-remote" % "1.2",
    "se.scalablesolutions.akka" % "akka-stm" % "1.2",
    "se.scalablesolutions.akka" % "akka-testkit" % "1.2"
)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0-SNAPSHOT")
