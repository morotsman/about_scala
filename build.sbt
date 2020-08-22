name := "generics"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.2"
libraryDependencies +=  "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.30"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1"
// libraryDependencies += "org.typelevel" %% "cats-concurrent" % "2.1.1"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.6.8"
