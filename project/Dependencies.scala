import sbt._

object Dependencies {

  private val versions = new {
    val scalameter = "0.19"
    val scalatest = "3.0.8"
  }

  private val scalameter = "com.storm-enroute" %% "scalameter" % versions.scalameter

  private val scalatest = "org.scalatest" %% "scalatest" % versions.scalatest

  val metius: Seq[ModuleID] = Seq(
    scalatest % Test
  )

  val benchmarks: Seq[ModuleID] = Seq(
    scalameter,
    scalatest
  )

}
