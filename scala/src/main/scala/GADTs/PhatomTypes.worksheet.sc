import Email.*
import Files.*

object Email:
  trait NotValidated
  trait Validated

  case class EmailAddress[ValidationStatus](address: String)

  def validateAddress(
      e: EmailAddress[NotValidated]
  ): Option[EmailAddress[Validated]] =
    if e.address matches "([0-9a-zA-Z-]+)@([0-9a-zA-Z-]+)\\.com"
    then Some(EmailAddress[Validated](e.address))
    else None

  def sendEmail(email: EmailAddress[Validated]): Unit = println("Email sent!")

  // val mkEmail: String => EmailAddress[NotValidated] = EmailAddress.apply

val notValidatedEmail = EmailAddress[NotValidated]("geza@example.com")

// val notValidatedEmail = mkEmail("geza@example.com")

val validatedEmail = validateAddress(notValidatedEmail).get // Don't do this!

sendEmail(validatedEmail)

// sendEmail(notValidatedEmail)

// EmailAddress[Validated]("nemvalid") // Make it opaque

object Files:
  protected sealed trait Open
  protected sealed trait Closed

  protected case class File[A](name: String)

  def openFile(name: String): File[Open] = File(name)
  def closeFile(file: File[Open]): File[Closed] = File[Closed](file.name)
  def readFile(file: File[Open]): String = "reading file"

val openedFile = openFile("test.txt")
val closedFile = closeFile(openedFile)
val content = readFile(openedFile)
