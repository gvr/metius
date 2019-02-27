import sbt._

object dependencies {

  private val versions = new {
    val scalameter = "0.10.1"
    val scalatest = "3.0.5"
  }

  private val test: Seq[ModuleID] = Seq(
    "com.storm-enroute" %% "scalameter" % versions.scalameter % "test,it",
    "org.scalatest" %% "scalatest" % versions.scalatest % "test,it"
  )

  val `metius-complex`: Seq[ModuleID] = test

  val `metius-random`: Seq[ModuleID] = test

}
