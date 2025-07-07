import scala.util.Try
import cats.data.StateT
import cats.*
import cats.effect.*
import cats.implicits.*
import Command.*
import Measurement.*
import cats.effect.unsafe.implicits.global

type ThermometerName = String

type BarometerName = String

enum Command:
  case ReadThermometer(name: ThermometerName)
  case ReadBarometer(name: BarometerName)
  case ReportTemperature
  case ReportAtmospherePressure
  case ClearData

type Script = List[Command]

enum Measurement:
  case TemperatureCelsius(
      name: ThermometerName,
      value: Float
  )
  case PressureAtmUnits(name: BarometerName, value: Float)

val script: Script =
  List(
    ReadThermometer("Garage"),
    ReadThermometer("Near the road"),
    ReadThermometer("House"),
    ReadBarometer("Garage"),
    ReadBarometer("House"),
    ReportTemperature,
    ReportAtmospherePressure,
    ClearData
  )

def interpreter(s: Script): IO[Unit] =
  helper(script).runA(List.empty)

def helper(
    script: Script
): StateT[IO, List[Measurement], Unit] = script match {
  case Nil => ().pure
  case cmd :: cmds =>
    for
      _ <- interpret(cmd)
      _ <- StateT.liftF(log(cmd))
      _ <- helper(cmds)
    yield ()
}

def isTemperature(m: Measurement): Boolean = m match {
  case TemperatureCelsius(_, _) => true
  case _                        => false
}

def isPressure(m: Measurement): Boolean = m match {
  case PressureAtmUnits(_, _) => true
  case _                      => false
}

val readTemp: IO[Float] = IO(12.2f)
val readBar: IO[Float] = IO(122.5f)

def interpret(
    cmd: Command
): StateT[IO, List[Measurement], Unit] = cmd match {
  case ClearData => StateT.set(List.empty)
  case ReadThermometer(name) =>
    for
      temp <- StateT.liftF(readTemp)
      m = TemperatureCelsius(name, temp)
      _ <- StateT.modify[IO, List[Measurement]](m :: _)
    yield ()
  case ReadBarometer(name) =>
    for
      bar <- StateT.liftF(readBar)
      m = PressureAtmUnits(name, bar)
      _ <- StateT.modify[IO, List[Measurement]](m :: _)
    yield ()
  case ReportTemperature =>
    for
      ms <- StateT.get
      temps = ms.filter(isTemperature)
      _ <- StateT.liftF(temps.traverse(IO.println))
    yield ()
  case ReportAtmospherePressure =>
    for
      ms <- StateT.get
      bars = ms.filter(isPressure)
      _ <- StateT.liftF(bars.traverse(IO.println))
    yield ()
}

def log = IO.println
