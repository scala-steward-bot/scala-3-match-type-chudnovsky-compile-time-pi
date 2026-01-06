package compile_time

sealed trait Comparison
final class GT extends Comparison
final class LT extends Comparison
final class EQ extends Comparison

object Comparison {
  type Equal[A <: Comparison, B <: Comparison] <: Boolean =
    A match {
      case GT =>
        B match {
          case GT =>
            true
          case _ =>
            false
        }
      case LT =>
        B match {
          case LT =>
            true
          case _ =>
            false
        }
      case EQ =>
        B match {
          case EQ =>
            true
          case _ =>
            false
        }
    }

  type Inverse[A <: Comparison] <: Comparison =
    A match {
      case GT => LT
      case EQ => EQ
      case LT => GT
    }
}
