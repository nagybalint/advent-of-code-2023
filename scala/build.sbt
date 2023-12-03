ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "nagybalint"

lazy val aoc23 = project
  .in(file("."))
  .settings(
    name := "Advent of Code 2023",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )