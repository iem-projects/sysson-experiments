    package at.iem.sysson.experiments.foo

    import scala.language.implicitConversions

    object GE {
      implicit def intIsGE   (x: Int   ): GE = ???
      implicit def doubleIsGE(x: Double): GE = ???

      implicit class GEOps(private val ge: GE) extends AnyVal {
        def <= (that: GE): GE = ???
        def >  (that: GE): GE = ???
        def &  (that: GE): GE = ???
        def poll(): Unit = ???
      }
    }
    trait GE

    object If {
      def apply(cond: => GE): IfBuilder = ???
    }

    trait IfBuilder {
      def Then [A](branch: => A): If[A]
    }

    trait If[A] {
      def Else: ElseBuilder[A]
      def ElseIf(cond: => GE): ElseIfBuilder[A]
    }

    trait IfGE extends If[GE] with GE

    object ElseBuilder {
      trait LowPri {
        implicit def AnyRes[A]: Result[A, If[A]] = ???  // !
      }
      object Result extends LowPri {
        implicit def GERes: Result[GE, IfGE] = ???
      }
      sealed trait Result[A, Out]
    }
    trait ElseBuilder[A] {
      def apply[B >: A, Out](branch: => B)(implicit result: ElseBuilder.Result[B, Out]): Out
    }

    trait ElseIfBuilder[A] {
      def Then [B >: A](branch: => B): If[B]
    }

    // example UGens
    case class Out(signal: GE)
    case class SinOsc(freq: GE) extends GE
    case class Dust(freq: GE) extends GE
    case class WhiteNoise() extends GE

    trait Tests {
      def freq: GE

      // ---- Unit/Any result ----

      val res0 = If (freq > 600) Then {
        Out(SinOsc(freq))
      }

      val res1 = If (freq > 400 & freq <= 600) Then {
        Out(SinOsc(freq))
      } Else {
        freq.poll()
      }

      // ---- GE result ----

      val res2: GE = If (freq > 100) Then {
        SinOsc(freq)
      } Else {
        WhiteNoise()
      }

      val res3: GE = If (freq > 1000) Then {
        SinOsc(freq)
      } ElseIf (freq > 100) Then {
        Dust(freq)
      } Else {
        WhiteNoise()
      }

      Out(res3)
    }