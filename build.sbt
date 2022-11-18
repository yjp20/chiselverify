// GENERAL
scalaVersion := "2.12.15"
scalacOptions := Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
)
lazy val chiselverify = (project in file("."))
Test / fork := true

// CHISEL
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.2" cross CrossVersion.full)
scalacOptions += "-P:chiselplugin:genBundleElements"
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.5.2",
  "edu.berkeley.cs" %% "chiseltest" % "0.5.2",
  "io.spray" %%  "spray-json" % "1.3.5"
)

// ASYNC
scalacOptions += "-Xasync"
libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.10.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

// BENCHMARK
enablePlugins(JmhPlugin)
