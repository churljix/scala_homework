package error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandling extends App {
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  case class PaymentCard(name: String, number: String, dateValidThru: String, verificationValue: String)

  sealed trait ValidationError
  object ValidationError {
    //nameOnCard
    final case object NameLengthError extends ValidationError {
      override def toString: String = "Card Holders name should be between 2 and 26 characters long"
    }
    final case object NameContainsNumbersError extends ValidationError {
      override def toString: String = "Card Holders name should not contain numbers"
    }
    //number
    final case object NumberEmptyError extends ValidationError {
      override def toString: String = "Card number must be entered"
    }
    final case object NumberLengthError extends ValidationError {
      override def toString: String = "Card number must contain exactly 16 numbers"
    }
    final case object NumberTypeError extends ValidationError {
      override def toString: String = "Card number must contain only numbers"
    }
    //dateValidThru
    final case object DateValidThruFormatError extends ValidationError {
      override def toString: String = "Date should contain / between month and year"
    }
    final case object DateValidThruMonthError extends ValidationError {
      override def toString: String = "Month out of range. Should be a number between 1 and 12"
    }
    final case object DateValidThruYearError extends ValidationError {
      override def toString: String = "Valid payment card year should be be number and not be in past"
    }
    final case object DateValidThruExpiredError extends ValidationError {
      override def toString: String = "Payment card is not valid. Entered Valid Thru date is in the past"
    }

    //verificationValue
    final case object VerificationValueError extends ValidationError {
      override def toString: String = "Verification code should be exactly 3 characters long"
    }

    final case object VerificationValueTypeError extends ValidationError {
      override def toString: String = "Verification code should contain only numbers"
    }
  }
  object PaymentCardValidator {
    import ValidationError._
    import java.time._
    import java.util.Calendar
    import java.text.SimpleDateFormat

    private def validateName(name: String): AllErrorsOr[String] = {

      def validateNameLength: AllErrorsOr[String] =
        if (name.length >= 2 && name.length <= 26) name.validNec
        else NameLengthError.invalidNec

      def validateNameContainsNumbers: AllErrorsOr[String] =
        if (name.matches("\\A\\p{ASCII}*\\z")) name.validNec
        else NameContainsNumbersError.invalidNec

      validateNameLength.productR(validateNameContainsNumbers)
    }

    private def validateNumber(number: String): AllErrorsOr[String] = {

      def validateNumberEmpty: AllErrorsOr[String] =
        if (number.length == 0) number.validNec
        else NumberEmptyError.invalidNec

      def validateNumberLength: AllErrorsOr[String] =
        if (number.length == 16) number.validNec
        else NumberLengthError.invalidNec

      def validateNumberType: AllErrorsOr[String] =
        if (number.matches("^[0-9]+$")) number.validNec
        else NumberTypeError.invalidNec

      validateNumberEmpty.productR(validateNumberLength).productR(validateNumberType)
    }

    private def validateDateValidThru(date: String): AllErrorsOr[String] = {

      def validateDateValidThruFormat: AllErrorsOr[String] =
        if (date.charAt(2) == '/') date.validNec
        else DateValidThruFormatError.invalidNec

      def validateDateValidThruMonth: AllErrorsOr[String] =
        if (1 to 12 contains date.substring(0,2).toInt) date.validNec
        else DateValidThruMonthError.invalidNec

      def validateDateValidThruYear: AllErrorsOr[String] =
        if (("20" + date.substring(3)).toInt >= Year.now.getValue) date.validNec
        else DateValidThruYearError.invalidNec

      def validateDateValidThruExpired: AllErrorsOr[String] = {
        val cal = Calendar.getInstance
        val dateTime = cal.getTime
        val dateFormat = new SimpleDateFormat("MM")
        val month = dateFormat.format(dateTime)

        if ((("20" + date.substring(3)).toInt > Year.now.getValue)
           || (date.substring(0,2).toInt >= month.toInt)) date.validNec
        else DateValidThruExpiredError.invalidNec}

      validateDateValidThruFormat.productR(validateDateValidThruMonth).productR(validateDateValidThruYear).productR(validateDateValidThruExpired)
    }

    private def validateVerificationValue(verificationValue: String): AllErrorsOr[String] = {

      def validateVerificationValue: AllErrorsOr[String] =
        if (verificationValue.length == 3) verificationValue.validNec
        else VerificationValueError.invalidNec

      def validateVerificationTypeValue: AllErrorsOr[String] =
        if (verificationValue.matches("^[0-9]+$")) verificationValue.validNec
        else VerificationValueTypeError.invalidNec

      validateVerificationValue.productR(validateVerificationTypeValue)
    }

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(name: String,
                 number: String,
                 expirationDate: String,
                 securityCode: String): AllErrorsOr[PaymentCard] = {
      (validateName(name),
       validateNumber(number),
       validateDateValidThru(expirationDate),
       validateVerificationValue(securityCode)).mapN(PaymentCard)
    }
  }
}