lazy val commonSettings = Seq(
  organization := "com.github.gvr",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
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
  }
)

lazy val publishSettings = Seq(
  publishArtifact in IntegrationTest := false,
  publishArtifact in Test := false,
  bintrayRepository := "mvn",
  pomIncludeRepository :=  { _ => false },
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  pomExtra in Global := {
    <url>https://github.com/gvr/metius</url>
      <scm>
        <connection>scm:git@github.com:gvr/metius.git</connection>
        <developerConnection>scm:git@github.com:gvr/metius.git</developerConnection>
        <url>https://github.com/gvr/metius</url>
      </scm>
      <developers>
        <developer>
          <id>gvr</id>
          <name>Ger van Rossum</name>
        </developer>
      </developers>
  }
)

lazy val metius = (project in file(".")).
  settings(commonSettings).
  settings(publishSettings).
  configs(IntegrationTest).
  settings(Defaults.itSettings).
  settings(
    name := "metius"
  )
