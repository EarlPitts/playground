import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util._

import cats.effect.kernel.Par
import cats.effect.kernel.instances.AllInstances
import cats.effect.testkit.*

import cats.effect.unsafe.implicits.global
import cats.effect.kernel.Outcome.*
import java.util.concurrent.Executor
import java.util.concurrent.Executors
import cats.effect.unsafe.IORuntime
import java.util.concurrent.CompletableFuture
import scala.io.Source
import scala.io.BufferedSource
implicit val ec: scala.concurrent.ExecutionContext =
  scala.concurrent.ExecutionContext.global

val a = (
  IO(println("1")),
  IO(println("2")),
  IO(println("3"))
).parTupled.void

val b = (
  IO(println("4")),
  IO(println("5")),
  IO(println("6"))
).parTupled.void

val p = (a,b).parTupled.void
p

p.unsafeRunSync()
