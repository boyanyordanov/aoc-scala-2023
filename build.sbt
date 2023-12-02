ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-scala-2023",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
  )
