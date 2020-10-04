import scala.io.Source

object ControlStructuresHw3 {

  sealed trait Command
  object Command {
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
    final case class Divide(dividend: Double, divisor: Double) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class ResultCalc(value: String) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import Command._

    x.toLowerCase().split(" +").toList match {
      case _ :: Nil   => Left(ErrorMessage("Command passed with incorrect input"))
      case x :: xs => x match {
          case "sum" => Right(Sum(xs.flatMap(s => scala.util.Try(s.toDouble).toOption)))
          case "average" => Right(Average(xs.flatMap(s => scala.util.Try(s.toDouble).toOption)))
          case "min" => Right(Min(xs.flatMap(s => scala.util.Try(s.toDouble).toOption)))
          case "max" => Right(Max(xs.flatMap(s => scala.util.Try(s.toDouble).toOption)))
          case "divide" if xs.length == 2 => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
          case _ => Left(ErrorMessage("Command passed with incorrect input"))
      }
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case Sum(numbers) => Right(ResultCalc(s"the sum of ${numbers.mkString(" ")} is ${numbers.sum}")) // sum 5 5 6 8.5 | the sum of 5 5 6 8.5 is 24.5
      case Average(numbers) => Right(ResultCalc(s"the average of ${numbers.mkString(" ")} is ${numbers.sum / numbers.length}")) // average 4 3 8.5 4 | the average of 4 3 8.5 4 is 4.875
      case Min(numbers) => Right(ResultCalc(s"the minimum of ${numbers.mkString(" ")} is ${numbers.min}")) // min 4 -3 -17 | the minimum of 4 -3 -17 is -17
      case Max(numbers) => Right(ResultCalc(s"the maximum  of ${numbers.mkString(" ")} is ${numbers.max}")) // max 4 -3 -17 | the maximum of 4 -3 -17 is 4
      case Divide(_, 0) => Left(ErrorMessage("Division with 0 not allowed"))
      case Divide(stNumber, ndNumber) => Right(ResultCalc(s"${stNumber} divided by ${ndNumber} is ${stNumber / ndNumber}")) // divide 4 5 | 4 divided by 5 is 0.8
      case _ => Left(ErrorMessage("Something went wrong"))
    }
  }

  def renderResult(x: Result): String = x match {
    case ResultCalc(result) => result
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result).fold(left => s"Error: ${left.value}", right => renderResult(right))
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

