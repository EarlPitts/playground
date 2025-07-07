lazy val http4sVersion = "0.23.26"
lazy val catsEffectVersion = "3.6.0"
lazy val catsVersion = "2.13.0"
lazy val kittensVersion = "3.5.0"

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion
libraryDependencies += "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion
libraryDependencies += "com.disneystreaming" %% "weaver-scalacheck" % "0.8.4" % Test
libraryDependencies += "org.typelevel" %% "kittens" % kittensVersion
libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.6.0"
libraryDependencies += "com.github.j-mie6" %% "parsley-cats" % "1.5.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "3.10.0"
libraryDependencies += "org.http4s" %% "http4s-ember-client" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-ember-server" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-circe" % http4sVersion
libraryDependencies += "io.circe" %% "circe-core" % "0.14.13"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.13"
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.13"
libraryDependencies += "io.circe" %% "circe-literal" % "0.14.13"
libraryDependencies += "io.circe" %% "circe-optics" % "0.15.0"
libraryDependencies += "org.typelevel" %% "log4cats-slf4j" % "2.3.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.6"
libraryDependencies += "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test


testFrameworks += new TestFramework("weaver.framework.CatsEffect")

scalaVersion := "3.6.4"

watchTriggeredMessage := ((a, b, c) => None)
watchStartMessage := ((a, b, c) => None)

Global / onChangedBuildSource := ReloadOnSourceChanges
Compile / run / fork := true

connectInput in run := true
