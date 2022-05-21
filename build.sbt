import Deps.*

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / semanticdbEnabled    := true // for metals

inThisBuild(
  Seq(
    scalaVersion     := "3.1.2",
    version          := "0.1.0",
    versionScheme    := Some("early-semver"),
    organization     := "thanh.se",
    organizationName := "Thanh Le",
    resolvers += Resolver.sonatypeRepo("snapshots"),

    // Github Workflow
    githubWorkflowPublishTargetBranches := Seq(), // Don't publish anywhere
    githubWorkflowBuild ++= Seq(
      WorkflowStep.Sbt(List("check"), name = Some("Check Formatting"))
    ),
    semanticdbVersion := scalafixSemanticdb.revision,

    // Scalafix
    scalafixDependencies += Libs.organizeImports
  )
)

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-source:future",
  scalacOptions += "-rewrite",
  scalacOptions += "-indent",
  libraryDependencies ++= Seq(
    Libs.cats,
    Libs.catsParse,
    Libs.munit,
    Libs.scalaCheck,
    Libs.munitScalaCheck
  ),
  resolvers += "jitpack" at "https://jitpack.io"
)

def full(p: Project) = p % "test->test;compile->compile"

val core = crossProject(JSPlatform, JVMPlatform)
  .settings(commonSettings, libraryDependencies += Libs.compression)

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(core.js, core.jvm)

// Commands
addCommandAlias("build", "prepare; test")
addCommandAlias("testAll", "all test")
addCommandAlias("prepare", "fix; fmt")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
addCommandAlias(
  "fixCheck",
  "; compile:scalafix --check ; test:scalafix --check"
)
addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("fmtCheck", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
addCommandAlias("check", "fixCheck; fmtCheck")
