name := """scala-lda"""

version := "1.0"

scalaVersion := "2.11.5"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalanlp" % "breeze_2.10" % "0.9"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9"

