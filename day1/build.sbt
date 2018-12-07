
lazy val root = (project in file("."))
  .settings(
    scalaVersion in ThisBuild := "2.12.1",
    version      in ThisBuild := "0.1.0-SNAPSHOT",
    name := "Advent",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    )
  )