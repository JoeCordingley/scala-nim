import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.6.1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.3.1"
  lazy val catsScalaCheck = "io.chrisdavenport" %% "cats-scalacheck" % "0.1.1"
}
