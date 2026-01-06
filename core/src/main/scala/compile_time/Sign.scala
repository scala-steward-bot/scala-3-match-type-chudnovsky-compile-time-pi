package compile_time

sealed trait Sign

object Sign {
  final class Pos extends Sign

  final class Zero extends Sign

  final class Neg extends Sign

  type Compare[A <: Sign, B <: Sign] <: Comparison =
    A match {
      case Pos =>
        B match {
          case Pos =>
            EQ
          case _ =>
            GT
        }
      case Zero =>
        B match {
          case Pos =>
            LT
          case Zero =>
            EQ
          case Neg =>
            GT
        }
      case Neg =>
        B match {
          case Neg =>
            EQ
          case _ =>
            LT
        }
    }

  type Equal[A <: Sign, B <: Sign] <: Boolean =
    A match {
      case Pos =>
        B match {
          case Pos =>
            true
          case _ =>
            false
        }
      case Neg =>
        B match {
          case Neg =>
            true
          case _ =>
            false
        }
      case Zero =>
        B match {
          case Zero =>
            true
          case _ =>
            false
        }
    }

  type Mult[A <: Sign, B <: Sign] <: Sign =
    A match {
      case Zero =>
        Zero
      case _ =>
        B match {
          case Zero =>
            Zero
          case _ =>
            Equal[A, B] match {
              case true =>
                Pos
              case false =>
                Neg
            }
        }
    }

  type Negate[A <: Sign] <: Sign =
    A match {
      case Zero =>
        Zero
      case Pos =>
        Neg
      case Neg =>
        Pos
    }

}
