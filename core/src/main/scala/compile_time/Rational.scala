package compile_time

import scala.compiletime.ops.double
import scala.compiletime.ops.int
import scala.compiletime.ops.string

sealed trait Rational

object Rational {
  final class Impl[A <: UInt, B <: UInt] extends Rational

  type _1 = Impl[UInt._1, UInt._1]

  type Inverse[A <: Rational] <: Rational =
    A match {
      case Impl[x, y] =>
        Impl[y, x]
    }

  type ToBinString[A <: Rational] <: String = A match {
    case Impl[h, t] =>
      string.+[
        string.+[
          UInt.ToString[h],
          " / "
        ],
        UInt.ToString[t]
      ]
  }

  type Add[A <: Rational, B <: Rational] <: Rational = A match {
    case Impl[h1, t1] =>
      UInt.Equal[h1, UInt._0] match {
        case true =>
          B
        case false =>
          B match {
            case Impl[h2, t2] =>
              UInt.Equal[h2, UInt._0] match {
                case true =>
                  A
                case false =>
                  Rational.Impl[
                    UInt.Add[
                      UInt.Mult[h1, t2],
                      UInt.Mult[t1, h2]
                    ],
                    UInt.Mult[t1, t2]
                  ]
              }
          }
      }
  }
}
