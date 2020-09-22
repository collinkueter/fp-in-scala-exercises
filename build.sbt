name := "fp-in-scala-exercises"
organization := "com.collinkueter"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {
  Seq(
    "org.typelevel" %% "cats-core" % "2.2.0-RC2",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.1.1" % Test,
  )
}

scalacOptions -= "-Xfatal-warnings"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")