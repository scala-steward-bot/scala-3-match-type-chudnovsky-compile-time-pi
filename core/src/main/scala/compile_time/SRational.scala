package compile_time

import scala.compiletime.ops.double
import scala.compiletime.ops.int
import scala.compiletime.ops.string

sealed trait SRational

object SRational {
  sealed trait Impl[A <: SInt, B <: UInt] extends SRational

  type _0 = Impl[SInt._0, UInt._1]

  type Add[A <: SRational, B <: SRational] <: SRational = A match {
    case Impl[h1, t1] =>
      SInt.Equal[h1, SInt._0] match {
        case true =>
          B
        case false =>
          B match {
            case Impl[h2, t2] =>
              SInt.Equal[h2, SInt._0] match {
                case true =>
                  A
                case false =>
                  SRational.Impl[
                    SInt.Add[
                      SInt.Mult[h1, SInt.FromUInt[t2]],
                      SInt.Mult[SInt.FromUInt[t1], h2]
                    ],
                    UInt.Mult[t1, t2]
                  ]
              }
          }
      }
  }

}
