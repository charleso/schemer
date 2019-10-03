import sbt._, Keys._

val hedgehog = Seq(
  "hedgehog" %% "hedgehog-sbt" % "59fa4de48083870452c2e949cb5fda5cc4f97256" % Test
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  skip in publish := true
)

lazy val schemer = Project(
    id = "schemer"
  , base = file(".")
  )
  .settings(standardSettings)
  .settings(noPublish)
  .aggregate(core, json, argonaut)

lazy val standardSettings = Seq(
    Defaults.coreDefaultSettings
  , projectSettings
  , compilationSettings
  ).flatten

lazy val projectSettings = Seq(
    name := "schemer"
  , version in ThisBuild := "1.0.0"
  , organization := "schemer"
  , scalaVersion := "2.12.7"
  , crossScalaVersions := Seq("2.10.7", "2.11.12", scalaVersion.value)
  , fork in run  := true
  )

lazy val core = Project(
    id = "core"
  , base = file("core")
  ).settings(standardSettings ++ Seq(
    name := "schemer-core"
  )).settings(libraryDependencies ++= hedgehog)

lazy val json = Project(
    id = "json"
  , base = file("json")
  ).settings(standardSettings ++ Seq(
    name := "schemer-json"
  )).dependsOn(core)


lazy val argonaut = Project(
    id = "argonaut"
  , base = file("argonaut")
  ).settings(standardSettings ++ Seq(
    name := "schemer-argonaut"
  )).dependsOn(core)

lazy val compilationSettings = Seq(
    maxErrors := 10
  , scalacOptions in Compile ++= Seq(
      "-deprecation"
    , "-unchecked"
    , "-feature"
    , "-language:_"
    , "-Ywarn-value-discard"
    , "-Yno-adapted-args"
    , "-Xlint"
    , "-Xfatal-warnings"
    ) ++ (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) =>
          Seq.empty
        case _ =>
          Seq("-Ywarn-unused-import")
      }
    )
  , scalacOptions in (Compile,console) := Seq("-language:_", "-feature")
  , scalacOptions in (Test,console) := Seq("-language:_", "-feature")
  , libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7" cross CrossVersion.binary)
  )

resolvers += Resolver.url("bintray-scala-hedgehog",
    url("https://dl.bintray.com/hedgehogqa/scala-hedgehog")
  )(Resolver.ivyStylePatterns)
