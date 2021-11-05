import sbt._

object Dependencies {
  lazy val testDeps = List(
    "org.scalatest" %% "scalatest" % "3.2.9",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0",
    "org.scalacheck" %% "scalacheck" % "1.15.4",
    "io.chrisdavenport" %% "cats-scalacheck" % "0.1.1"
  ).map(_ % Test)
  lazy val cats = "org.typelevel" %% "cats-core" % "1.6.1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.3.1"
  lazy val all = cats :: catsEffect :: testDeps
}
