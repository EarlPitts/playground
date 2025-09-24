import weaver._

import cats.effect._
import ErrorHandling.MonadThrow.*
import ErrorHandling.MonadThrow.UserThrowable.*

// Suites must be "objects" for them to be picked by the framework
object MonadThrowSuite extends FunSuite {

  val address = "address"
  val validAge = 30
  val validName = "Bela"

  val invalidAge = 120
  val invalidName = "Verylongname"
  val existingName = "Janos"

  // Happy path
  test("every input is valid") {
    expect(
      makeUser(validAge, validName, address) ==
        Right(User(1, validAge, validName, address))
    )
  }

  test("invalid age") {
    expect(
      makeUser(invalidAge, validName, address) ==
        Left(InvalidAge)
    )
  }

  test("invalid name") {
    expect(
      makeUser(validAge, invalidName, address) ==
        Left(InvalidName)
    )
  }

  test("existing name") {
    expect(
      makeUser(validAge, existingName, address) ==
        Left(AlreadyInDB)
    )
  }
}
