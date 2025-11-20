ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "API-Assignment",
    libraryDependencies ++= Seq(
      // HTTP requests library
      "com.lihaoyi" %% "requests" % "0.8.0",
      // JSON parsing library
      "com.lihaoyi" %% "ujson" % "3.1.3"
    )
  )