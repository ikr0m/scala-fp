name := "fp-examples"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

val catsVersion = "2.0.0-M1"
val simulacrumVersion = "0.18.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "com.github.mpilquist" %% "simulacrum" % simulacrumVersion,
  "io.monix" %% "monix" % "3.0.0-RC3",
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1")