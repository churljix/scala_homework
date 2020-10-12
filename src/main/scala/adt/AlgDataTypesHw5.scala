package adt

object AlgDataTypesHw5 extends App {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type

  // 1. Suit
  final case class Suit private (suit: String) extends AnyVal
  object Suit {
    def create(handSuit: String): Either[String, Suit] = handSuit match {
      case "d" | "c" | "h" | "s" => Right(Suit(handSuit))
      case _ => Left("Invalid Suit")
    }
  }

  // 2. Rank
  final case class Rank private (suit: String) extends AnyVal
  object Rank {
    def create(handRank: String): Either[String, Rank] = handRank match {
      case "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "T" | "J" | "Q" | "K" | "A" => Right(Rank(handRank))
      case _ => Left("Invalid Rank")
    }
  }
  // 3. Card
  final case class Card(rank: Rank, suit: Suit)

  // 4. Hand (Texas or Omaha)
  final case class Hand(hand: List[Card])
  object Hand {
    def pokerType(hand: List[Card]): Either[String, String] = hand.size match {
      case 2 => Right("Texas")
      case 4 => Right("Omaha")
      case _ => Left("Invalid Hand")
    }
  }

  // 5. Board
  final case class Board private (board: List[Card])
  object Board {
    def check(board: List[Card]): Either[String, Board] = board.size match {
      case 5 => Right(Board(board))
      case _ => Left("Invalid Board")
    }
  }

  // 6. Poker Combination (High Card, Pair, etc.)
  sealed trait PokerCombinations
  object PokerCombinations {
    final case object HighCard extends PokerCombinations
    final case object Pair extends PokerCombinations
    final case object TwoPair extends PokerCombinations
    final case object ThreeOfAKind extends PokerCombinations
    final case object Straight extends PokerCombinations
    final case object Flush extends PokerCombinations
    final case object FullHouse extends PokerCombinations
    final case object FourOfAKind extends PokerCombinations
    final case object StraightFlush extends PokerCombinations
    final case object RoyalFlush extends PokerCombinations
  }

  // 7. Test Case (Board & Hands to rank)
  final case class TestCase (cards: List[Card], board: List[Board])

  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  final case class TestResult (result: List[Card])

}
