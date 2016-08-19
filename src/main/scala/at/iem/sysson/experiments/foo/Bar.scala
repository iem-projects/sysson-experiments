package at.iem.sysson.experiments.foo

trait Bar {
    trait GE
    trait If[A] {
      def Else[B >: A, Out](cond: => B)(implicit result: Result[B, Out]): Out
    }
    trait IfGE extends If[GE] with GE

    case class SinOsc()     extends GE
    case class WhiteNoise() extends GE

    trait LowPri {
      implicit def AnyRes[A]: Result[A, If[A]] = ???  // !
    }
    object Result extends LowPri {
      implicit def GERes: Result[GE, IfGE] = ???
    }
    sealed trait Result[-A, Out]

    def IfExample: If[SinOsc]

    val res0: GE = IfExample.Else[GE, IfGE](WhiteNoise())
    val res1: GE = IfExample.Else          (WhiteNoise())
}
