import sbt._

object Library {
  val testDependencies =
    Seq( "org.scalatest"  %% "scalatest"  % "3.2.14" % "test" )
}