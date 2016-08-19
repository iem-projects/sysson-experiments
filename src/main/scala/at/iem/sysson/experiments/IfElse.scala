package at.iem.sysson.experiments

import de.sciss.synth
import de.sciss.synth.GE

import scala.language.implicitConversions

/*

  variant 1 (future Scala)

  If (cond) Then xxx
  If (cond) Then xxx Else If (cond) Then yyyy
  If (cond) Then xxx Else If (cond) Then yyyy Else zzz

  variant 2 (current Scala)

  If (cond) xxx
  If (cond) xxx Else If (cond) yyyy
  If (cond) xxx Else If (cond) yyyy Else zzz

  The advantage of variant 1 is that we can write a graph element
  directly as `xxx`, whereas in variant 2 this would be syntactically
  forbidden and would require curly braces to invoke `apply`. In other
  words, variant 2 while looking more like regular current Scala does
  not cover all possibilities of regular Scala syntax, while variant 1
  might become possible in a future Scala version, although without
  requiring parentheses around the initial condition:

  http://docs.scala-lang.org/sips/pending/uncluttering-control.html

  // ---- Unit result ----

  If (freq > 100) Then {
    Out.ar(0, SinOsc.ar(freq))
  }

  If (freq > 100) Then {
    Out.ar(0, SinOsc.ar(freq))
  } Else {
    freq.poll(0, "freq")
  }

  // ---- GE result ----

  val res: GE = If (freq > 100) Then {
    SinOsc.ar(freq)
  } Else {
    WhiteNoise.ar
  }

  val res: GE = If (freq > 1000) Then {
    SinOsc.ar(freq)
  } ElseIf (freq > 100) Then {
    Dust.ar(freq)
  } Else {
    WhiteNoise.ar
  }


 */

object If {
  def apply(cond: => GE): IfBuilder = ???

//  implicit def result[A](x: If[A]): A = ???
}

trait IfBuilder {
  def Then [A](branch: => A): If[A]

  // def apply[A](branch: => A): If[A] = Then(branch)
}

trait If[A] {
  def Else: ElseBuilder[A]
  def ElseIf(cond: => GE): ElseIfBuilder[A]
}

trait IfGE extends If[GE] with GE

object ElseBuilder {
  trait LowPri {
    implicit def Unit[A]: Result[A, If[A]] = ???
  }
  object Result extends LowPri {
    implicit def GE: Result[synth.GE, IfGE] = ???
  }
  sealed trait Result[-A, Out /* <: If[A] */]
}
trait ElseBuilder[A] {
  def apply[B >: A, Out](branch: => B)(implicit result: ElseBuilder.Result[B, Out]): Out
}

trait ElseIfBuilder[A] {
  def Then [B >: A](branch: => B): If[B]

  // def apply[B >: A](branch: => B): If[B] = Then(branch)
}

//object If {
//  def ir(cond: GE): If = new If(cond)
//}
//final case class If(cond: GE) extends GE.Lazy  {
//
//  def rate: MaybeRate = ???
//
//  protected def makeUGens: UGenInLike = {
//    val numChannels: Int = ???
//    val dc = DC.ar(0)
//    val ge: GE = if (numChannels == 1) dc else Seq.fill(numChannels)(dc)
//    ge
//  }
//
//  def Else
//}
