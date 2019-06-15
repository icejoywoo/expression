name := "expression"
organization := "org.icejoywoo"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.twitter" %% "util-eval" % "6.43.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"