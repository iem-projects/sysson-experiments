package at.iem.sysson.experiments

import de.sciss.synth
import de.sciss.synth.ugen.impl.modular.{IfBuilderImpl => IfMod}
import de.sciss.synth.ugen.impl.monolithic.{IfBuilderImpl => IfMono}
import de.sciss.synth.{ControlRated, GE, SysSonUGenGraphBuilder, UGenGraph, UGenInLike}

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
  var monolithic: Boolean = true

  def apply(cond: GE): IfBuilder = if (If.monolithic) IfMono(cond) else IfMod(cond)
}

object IfLag {
  def apply(cond: GE, dur: GE): IfBuilder = ??? // if (If.monolithic) IfMono(cond) else IfMod(cond)
}

trait IfBuilder {
  def Then [A](branch: => A): If[A]
}

trait If[A] {
  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilder.Result[B, Out]): Out
  def ElseIf (cond: GE): ElseIfBuilder[A]
}

object ElseBuilder {
  object Result extends LowPri {
    implicit def GE: ElseBuilder.GE.type = ElseBuilder.GE
  }
  sealed trait Result[-A, Out]

  object GE           extends Result[synth.GE, synth.GE  ]
  final class Unit[A] extends Result[A       , scala.Unit]

  trait LowPri {
    implicit final def Unit[A]: Unit[A] = instance.asInstanceOf[Unit[A]]
    private final val instance = new Unit[Any]
  }
}

trait ElseIfBuilder[A] {
  def Then [B >: A](branch: => B): If[B]
}

final case class ThisBranch() extends GE.Lazy with ControlRated {
  protected def makeUGens: UGenInLike =
    UGenGraph.builder match {
      case sysson: SysSonUGenGraphBuilder => sysson.thisBranch
      case _ => sys.error(s"Cannot expand ThisBranch outside of SysSon UGen graph builder")
    }
}