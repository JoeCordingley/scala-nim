import Dependencies._

ThisBuild / scalaVersion := "2.12.0"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

lazy val root = (project in file("."))
  .settings(
    name := "scala-nim",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test,
      catsScalaCheck % Test,
      cats,
      catsEffect
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
