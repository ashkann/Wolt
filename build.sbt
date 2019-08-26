name := "Wolt"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.3.5",
  "com.typesafe.akka" %% "akka-actor" % "2.6.0-M6",
  "com.typesafe.akka" %% "akka-stream" % "2.6.0-M6",
  "com.typesafe.akka" %% "akka-http"   % "10.1.9"
)

enablePlugins(JavaServerAppPackaging)
enablePlugins(DockerPlugin)
