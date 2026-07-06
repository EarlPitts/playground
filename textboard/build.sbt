val Http4sVersion = "0.23.32"
val CirceVersion = "0.14.15"
val MunitVersion = "1.1.1"
val LogbackVersion = "1.5.18"
val MunitCatsEffectVersion = "2.1.0"

lazy val root = (project in file("."))
  .settings(
    organization := "home",
    name := "textboard",
    scalaVersion := "3.8.4",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % CirceVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "io.circe" %% "circe-parser" % CirceVersion,
      "org.http4s" %% "http4s-ember-server" % Http4sVersion,
      "org.http4s" %% "http4s-ember-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.scalameta" %% "munit" % MunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % MunitCatsEffectVersion % Test,
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Runtime,
      "org.tpolecat" %% "doobie-core"   % "1.0.0-RC8",
      "org.tpolecat" %% "doobie-hikari" % "1.0.0-RC8",
      "org.xerial" % "sqlite-jdbc" % "3.45.0.0"
    )
  )
