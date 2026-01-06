package compile_time

import compile_time.Digit.One
import compile_time.Digit.Zero
import scala.compiletime.ops.any
import scala.compiletime.ops.double
import scala.compiletime.ops.int
import scala.compiletime.ops.long
import scala.compiletime.ops.string

sealed trait UInt

final class DCons[D <: Digit, T <: UInt] extends UInt

final class DNil extends UInt

object UInt {

  type Mult2[A <: UInt] <: UInt =
    A match {
      case _0 =>
        _0
      case _ =>
        DCons[Digit.Zero, A]
    }

  type Rem[A <: UInt, B <: UInt] =
    Rem0[A, B, _0]

  type Rem0[A <: UInt, B <: UInt, C <: UInt] <: UInt =
    Compare[A, Add[C, B]] match {
      case GT =>
        Rem0[A, B, Add[C, B]]
      case EQ =>
        UInt._0
      case LT =>
        Minus[A, C]
    }

  type Minus[A <: UInt, B <: UInt] <: UInt =
    A match {
      case DNil =>
        B match {
          case DNil =>
            DNil
        }
      case DCons[h1, t1] =>
        B match {
          case DNil =>
            A
          case DCons[h2, t2] =>
            h1 match {
              case One =>
                h2 match {
                  case One =>
                    Mult2[
                      Minus[t1, t2]
                    ]
                  case Zero =>
                    DCons[One, Minus[t1, t2]]
                }
              case Zero =>
                h2 match {
                  case One =>
                    DCons[One, Minus[Dec[t1], t2]]
                  case Zero =>
                    Mult2[
                      Minus[t1, t2]
                    ]
                }
            }
        }
    }

  type Exp[A <: UInt, B <: UInt] <: UInt =
    A match {
      case DNil =>
        B match {
          case DNil =>
            _1
          case DCons[?, ?] =>
            _0
        }
      case DCons[?, ?] =>
        ReceiveExp[B, A, _1]
    }

  type ReceiveExp[A <: UInt, B <: UInt, ACC <: UInt] <: UInt = A match {
    case DNil =>
      ACC
    case DCons[One, t] =>
      ReceiveExp[
        t,
        Sq[B],
        Mult[ACC, B]
      ]
    case DCons[Zero, t] =>
      ReceiveExp[
        t,
        Sq[B],
        ACC
      ]
  }

  type Sq[A <: UInt] <: UInt = A match {
    case DNil =>
      DNil
    case DCons[?, ?] =>
      ReceiveMult[A, A, DNil]
  }

  type Mult[A <: UInt, B <: UInt] <: UInt = A match {
    case DNil =>
      DNil
    case DCons[h, t] =>
      Compare[B, A] match {
        case LT =>
          ReceiveMult[B, A, DNil]
        case _ =>
          ReceiveMult[A, B, DNil]
      }
  }

  type <[A <: UInt, B <: UInt] <: Boolean = Compare[A, B] match {
    case LT =>
      true
    case EQ =>
      false
    case GT =>
      false
  }

  type Compare[A <: UInt, B <: UInt] = CompareC[A, B, EQ]

  type CompareC[A <: UInt, B <: UInt, Carry <: Comparison] <: Comparison = A match {
    case DNil =>
      B match {
        case DNil =>
          Carry
        case DCons[?, ?] =>
          LT
      }
    case DCons[h, t] =>
      B match {
        case DNil =>
          GT
        case DCons[?, ?] =>
          CompareNZ[A, B, Carry]
      }
  }

  type CompareNZ[A <: UInt, B <: UInt, C <: Comparison] <: Comparison = A match {
    case DCons[h1, t1] =>
      B match {
        case DCons[h2, t2] =>
          CompareC[
            t1,
            t2,
            Carry[Digit.Compare[h1, h2], C]
          ]
      }
  }

  type Carry[New <: Comparison, C <: Comparison] <: Comparison = New match {
    case LT => LT
    case GT => GT
    case EQ => C
  }

  type ReceiveMult[A <: UInt, shifted <: UInt, acc <: UInt] <: UInt = A match {
    case DNil =>
      acc
    case DCons[One, tail] =>
      ReceiveMult[
        tail,
        Mult2[shifted],
        Add[acc, shifted]
      ]
    case DCons[Zero, tail] =>
      ReceiveMult[
        tail,
        Mult2[shifted],
        acc
      ]
  }

  type ToString[A <: UInt] = ToString0[A, ""]

  type Equal[A <: UInt, B <: UInt] <: Boolean = Compare[A, B] match {
    case EQ => true
    case GT => false
    case LT => false
  }

  type ToString0[A <: UInt, B <: String] <: String = A match {
    case DNil =>
      B
    case DCons[h, t] =>
      ToString0[t, string.+[Digit.ToString[h], B]]
  }

  type FromInt[A <: Int] <: UInt = int.>=[A, 0] match {
    case true =>
      A match {
        case 0 =>
          _0
        case 1 =>
          _1
        case 2 =>
          _2
        case _ =>
          int.%[A, 2] match {
            case 0 =>
              Mult2[
                FromInt[int./[A, 2]]
              ]
            case 1 =>
              DCons[One, FromInt[int./[A, 2]]]
          }
      }
  }

  type Inc[A <: UInt] <: UInt = A match {
    case DNil =>
      DCons[One, DNil]
    case DCons[h, t] =>
      h match {
        case One =>
          DCons[Zero, Inc[t]]
        case Zero =>
          DCons[One, t]
      }
  }

  type Add[A <: UInt, B <: UInt] <: UInt = B match {
    case DCons[h1, t1] =>
      A match {
        case DNil =>
          B
        case DCons[h2, t2] =>
          AddNZ[A, B]
      }
    case DNil =>
      A
  }

  type AddNZ[A <: UInt, B <: UInt] <: UInt = A match {
    case DCons[One, t] =>
      Add1[A, B]
    case DCons[Zero, t1] =>
      B match {
        case DCons[h, t2] =>
          DCons[h, Add[t1, t2]]
      }
  }

  type Add1[A <: UInt, B <: UInt] <: UInt = B match {
    case DCons[One, t1] =>
      A match {
        case DCons[h, t2] =>
          DCons[Zero, Inc[Add[t2, t1]]]
      }
    case DCons[Zero, t1] =>
      A match {
        case DCons[h, t2] =>
          DCons[h, Add[t2, t1]]
      }
  }

  type Dec[A <: UInt] <: UInt = A match {
    case DCons[h, t] =>
      h match {
        case One =>
          t match {
            case DNil =>
              DNil
            case _ =>
              DCons[Zero, t]
          }
        case Zero =>
          DCons[One, Dec[t]]
      }
  }

  type Factorial[A <: UInt] =
    Factorial0[A, UInt._1]

  type Factorial0[A <: UInt, B <: UInt] <: UInt =
    UInt.<[A, UInt._1] match {
      case true =>
        B
      case false =>
        Factorial0[
          UInt.Dec[A],
          UInt.Mult[
            A,
            B
          ]
        ]
    }

  private type ::[H <: Digit, T <: UInt] = DCons[H, T]

  type _0 = DNil
  type _1 = One :: DNil
  type _2 = Zero :: One :: DNil
  type _3 = One :: One :: DNil
  type _4 = Zero :: Zero :: One :: DNil
  type _5 = One :: Zero :: One :: DNil
  type _6 = Zero :: One :: One :: DNil

}
