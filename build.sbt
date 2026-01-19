name := """calculator"""
organization := "com.compiler-lab3"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "3.7.2"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"

resolvers += "luzhuomi github repo" at "https://raw.githubusercontent.com/luzhuomi/mavenrepo/master/"

libraryDependencies += "org.ow2.asm" % "asm" % "9.6"

libraryDependencies += "org.scalactic" % "scalactic_3" % "3.2.10"

libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.10" % "test"

libraryDependencies += "org.scalatest" % "scalatest-funsuite_3" % "3.2.10" % "test"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.compiler-lab3.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.compiler-lab3.binders._"
