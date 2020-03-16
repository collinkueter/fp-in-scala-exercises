ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "com.collinkueter"

lazy val hello = (project in file("."))
  .settings(
    name := "fp-in-scala-exercises",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test,
  )
