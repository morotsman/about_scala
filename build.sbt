name := "generics"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.2"
libraryDependencies +=  "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.30"
