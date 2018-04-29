lazy val commonSettings = Seq(
  organization := "cen.alpha",
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Xfuture",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused"
  ),
  libraryDependencies ++= {
    val scalaTestVersion = "3.0.5"
    val scalaMeterVersion = "0.9"
    Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test,it",
      "com.storm-enroute" %% "scalameter" % scalaMeterVersion % "it"
    )
  },
  publishMavenStyle := true,
  publishTo := Some("Bintray API Realm" at s"https://api.bintray.com/content/gvr/cen.alpha/metius/${version.value}")
)

lazy val metius = (project in file(".")).
  settings(commonSettings).
  configs(IntegrationTest).
  settings(Defaults.itSettings).
  settings(
    name := "metius"
  )
