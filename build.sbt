name := "fp-examples"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

val catsVersion = "2.0.0-M1"
val simulacrumVersion = "0.18.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "com.github.mpilquist" %% "simulacrum" % simulacrumVersion
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
