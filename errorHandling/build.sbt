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

scalaVersion := "3.7.3"

watchTriggeredMessage := ((a, b, c) => None)
watchStartMessage := ((a, b, c) => None)

Global / onChangedBuildSource := ReloadOnSourceChanges
Compile / run / fork := true

connectInput in run := true

scalacOptions ++= Seq(
  "-unchecked",                     // Enable additional warnings where generated code depends on assumptions
  // "-deprecation",                   // Emit warning and location for usages of deprecated APIs
  // "-feature",                       // Emit warning and location for usages of features that should be imported explicitly
  // "-Wvalue-discard",               // Warn when non-Unit expression results are unused
  // "-Wnonunit-statement",           // Warn when statements are not expressions
  // "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver
  // "-Xlint:nullary-unit",           // Warn when nullary methods return Unit
  // "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures
  // "-Xlint:nullary-override",       // Warn when non-nullary `def f()' overrides nullary `def f'
  // "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`
  // "-Xlint:missing-interpolator",   // Warn when literal strings contain ` $ ` or ` $$` but are not interpolated
  // "-Xlint:doc-detached",           // Warn when doc comments are discarded
  // "-Xlint:private-shadow",         // Warn when a private field (or class parameter) shadows a superclass field
  // "-Xlint:type-parameter-shadow",  // Warn when a local type parameter shadows a type already in scope
  // "-Xlint:poly-implicit-overload", // Warn when parameterized overloaded implicit methods are not visible as view bounds
  // "-Xlint:option-implicit",        // Warn when Option.apply is used implicitly
  // "-Xlint:delayedinit-select",     // Warn when selecting member of DelayedInit
  // "-Xlint:package-object-classes", // Warn when package object contains class or interface definitions
  // "-Xlint:stars-align",            // Warn when using stars in type ascriptions
  // "-Xlint:constant",               // Warn when evaluating constant arithmetic
  // "-Xlint:unused",                 // Enable -Wunused:imports,locals,privates,implicits
  // "-Xlint:nonlocal-return",        // Warn when return statements are used in functions
  // "-Xlint:implicit-not-found",     // Warn when implicit resolution cannot find an implicit value
  // "-Xlint:serial",                 // Warn about Serializable classes with no serialVersionUID
  // "-Xlint:valpattern",             // Warn on pattern matches in val definitions
  // "-Xlint:eta-zero",               // Warn on eta-expansion of zero-ary method
  // "-Xlint:eta-sam",                // Warn on eta-expansion to meet a Single Abstract Method interface
  // "-Xlint:deprecation"             // Warn about deprecated features
)
