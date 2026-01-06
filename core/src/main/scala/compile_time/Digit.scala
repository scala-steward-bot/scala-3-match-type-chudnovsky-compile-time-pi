package compile_time

sealed trait Digit

object Digit {
  sealed trait One extends Digit
  sealed trait Zero extends Digit

  type ToString[A] <: String = A match {
    case Digit.One => "1"
    case Digit.Zero => "0"
  }

  type Compare[A <: Digit, B <: Digit] <: Comparison = A match {
    case Digit.One =>
      B match {
        case Digit.One =>
          EQ
        case Digit.Zero =>
          GT
      }
    case Digit.Zero =>
      B match {
        case Digit.One =>
          LT
        case Digit.Zero =>
          EQ
      }
  }
}
