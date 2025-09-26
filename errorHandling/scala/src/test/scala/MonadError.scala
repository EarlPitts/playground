import weaver._

import cats.effect._
import ErrorHandling.MonadError.*
import ErrorHandling.MonadError.UserErr.*

// Suites must be "objects" for them to be picked by the framework
object MonadErrorSuite extends SimpleIOSuite {

  val address = "address"
  val validAge = 30
  val validName = "Bela"

  val invalidAge = 120
  val invalidName = "Verylongname"
  val existingName = "Janos"

  // Happy path
  test("every input is valid") {
    makeUser(validAge, validName, address).value.map(u =>
      expect(
        u == Right(User(1, validAge, validName, address))
      )
    )
  }
}
