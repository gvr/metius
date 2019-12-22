
inThisBuild(
  Seq(
    organization := "com.github.gvr",
    scalaVersion := "2.13.1",
    crossScalaVersions := Seq(scalaVersion.value, "2.12.10", "2.11.12"),
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xlint",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused"
    )
  )
)

lazy val publishSettings = Seq(
  publishArtifact in Test := false,
  bintrayRepository := "mvn",
  pomIncludeRepository := { _ => false },
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

lazy val root = (project in file("."))
  .aggregate(metius, examples, benchmarks)
  .settings(
    name := "metius"
  )

lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(metius)
  .settings(
    name := "benchmarks",
    libraryDependencies ++= Dependencies.benchmarks
  )

lazy val examples = (project in file("examples"))
  .dependsOn(metius)
  .settings(
    name := "examples"
  )

lazy val metius = (project in file("metius")).
  settings(publishSettings).
  settings(
    name := "metius",
    libraryDependencies ++= Dependencies.metius
  )
