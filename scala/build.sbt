name := "N2S"

version := "1.0"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

libraryDependencies += "com.typesafe.akka" % "akka-remote" % "2.0"

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"
