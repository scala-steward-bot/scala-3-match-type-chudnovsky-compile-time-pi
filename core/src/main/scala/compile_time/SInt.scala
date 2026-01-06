package compile_time

import scala.compiletime.ops.int
import scala.compiletime.ops.long
import scala.compiletime.ops.string

/**
  * signed integer
  */
sealed trait SInt

object SInt {
  final class Impl[A <: Sign, B <: UInt] extends SInt

  type Zero = Impl[Sign.Zero, UInt._0]

  type _0 = Zero
  type _1 = Impl[Sign.Pos, UInt._1]
  type _2 = Impl[Sign.Pos, UInt._2]

  type S[A <: SInt] <: Sign =
    A match {
      case Impl[s, ?] =>
        s
    }

  type Value[A <: SInt] <: UInt =
    A match {
      case Impl[?, x] =>
        x
    }

  type Equal[A <: SInt, B <: SInt] <: Boolean =
    Sign.Equal[S[A], S[B]] match {
      case true =>
        UInt.Equal[
          Value[A],
          Value[B]
        ]
      case false =>
        false
    }

  type Mult[A <: SInt, B <: SInt] =
    Impl[
      Sign.Mult[
        S[A],
        S[B]
      ],
      UInt.Mult[
        Value[A],
        Value[B]
      ]
    ]

  type FromUInt[A <: UInt] <: SInt =
    A match {
      case UInt._0 =>
        Zero
      case _ =>
        Impl[Sign.Pos, A]
    }

  type Add[A <: SInt, B <: SInt] <: SInt =
    S[A] match {
      case Sign.Zero =>
        B
      case _ =>
        S[B] match {
          case Sign.Zero =>
            A
          case _ =>
            Sign.Compare[S[A], S[B]] match {
              case EQ =>
                SInt.Impl[
                  S[A],
                  UInt.Add[
                    Value[A],
                    Value[B]
                  ]
                ]
              case _ =>
                UInt.Compare[Value[A], Value[B]] match {
                  case EQ =>
                    Zero
                  case LT =>
                    Impl[
                      AddSign[A, B],
                      UInt.Minus[
                        Value[B],
                        Value[A]
                      ]
                    ]
                  case GT =>
                    Impl[
                      AddSign[A, B],
                      UInt.Minus[
                        Value[A],
                        Value[B]
                      ]
                    ]
                }
            }
        }
    }

  type AddSign[A <: SInt, B <: SInt] <: Sign =
    Comparison.Equal[
      Sign.Compare[
        S[A],
        S[B]
      ],
      UInt.Compare[
        Value[A],
        Value[B]
      ]
    ] match {
      case true =>
        Sign.Pos
      case false =>
        Sign.Neg
    }

  type Negate[A <: SInt] =
    Impl[
      Sign.Negate[S[A]],
      Value[A]
    ]
}
