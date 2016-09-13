package de.sciss.synth
package ugen

import de.sciss.synth

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
//  var monolithic: Boolean = true

//  def apply(cond: GE): IfBuilderT =
//    if (If.monolithic) IfMono(cond = cond, lagTime = Constant.C0)
//    else               IfMod (cond = cond, lagTime = Constant.C0)
}
final case class If(cond: GE) {
  def Then [A](branch: => A): IfThen[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    IfThen(cond, g)(res)
  }
}

//object IfLag {
//  def apply(cond: GE, dur: GE): IfBuilderT =
//    if (If.monolithic) IfMono(cond = cond, lagTime = dur)
//    else               IfMod (cond = cond, lagTime = dur)
//}

final case class IfLag(cond: GE, dur: GE) {
  def Then [A](branch: => A): IfLagThen[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    IfLagThen(cond = cond, dur = dur, branch = g)(res)
  }
}

sealed trait Then[+A] extends Lazy {
  // this acts now as a fast unique reference
  @transient final protected lazy val ref = new AnyRef

  // ---- constructor ----
  SynthGraph.builder.addLazy(this)

  def cond  : GE
  def branch: SynthGraph
  def result: A

  private[synth] final def force(b: UGenGraph.Builder): Unit = UGenGraph.builder match {
    case nb: NestedUGenGraphBuilder => visit(nb)
    case _ => sys.error(s"Cannot expand modular IfGE outside of NestedUGenGraphBuilder")
  }

  private[synth] final def visit(nb: NestedUGenGraphBuilder): NestedUGenGraphBuilder.ExpIfCase =
    nb.visit(ref, nb.expandIfCase(this))
}

sealed trait IfOrElseIfThen[+A] extends Then[A] {
  import ugen.{Else => _Else} // really, Scala?
  def Else [B >: A, Out](branch: => B)(implicit result: _Else.Result[B, Out]): Out = result.make(this, branch)
}

sealed trait IfThenLike[+A] extends IfOrElseIfThen[A] {
  def dur: GE

  def ElseIf (cond: GE): ElseIf[A] = new ElseIf(this, cond)
}

final case class IfThen[A](cond: GE, branch: SynthGraph)(val result: A)
  extends IfThenLike[A]
  with Lazy {

  def dur: GE = Constant.C0

//  protected def makeUGens: Unit =
//    UGenGraph.builder match {
//      case sysson: SysSonUGenGraphBuilder =>
//        sysson.expandIfCase(cond = cond, lagTime = Constant.C0, branch = branch)
//
//      case _ => sys.error(s"Cannot expand modular IfGE outside of SysSon UGen graph builder")
//    }
}
final case class IfLagThen[A](cond: GE, dur: GE, branch: SynthGraph)(val result: A)
  extends IfThenLike[A]

final case class ElseIf[+A](pred: IfOrElseIfThen[A], cond: GE) {
  def Then [B >: A](branch: => B): ElseIfThen[B] = {
    var res: B = null.asInstanceOf[B]
    val g = SynthGraph {
      res = branch
    }
    ElseIfThen[B](pred, cond, g)(res)
  }
}

sealed trait ElseOrElseIfThen[+A] extends Then[A] {
  def pred: IfOrElseIfThen[A]
}

final case class ElseIfThen[+A](pred: IfOrElseIfThen[A], cond: GE, branch: SynthGraph)(val result: A)
  extends IfOrElseIfThen[A] with ElseOrElseIfThen[A] {

  def ElseIf (cond: GE): ElseIf[A] = new ElseIf(this, cond)
}

object Else {
  object Result extends LowPri {
    implicit def GE: Else.GE.type = Else.GE
  }
  sealed trait Result[-A, Out] {
    def make(pred: IfOrElseIfThen[A], branch: => A): Out
  }

  object GE extends Result[synth.GE, ElseGE] {
    def make(pred: IfOrElseIfThen[GE], branch: => GE): ElseGE = {
      var res: GE = null
      val g = SynthGraph {
        res = branch
      }
      ElseGE(pred, g)(res)
    }
  }

  final class Unit[A] extends Result[A, ElseUnit] {
    def make(pred: IfOrElseIfThen[A], branch: => A): ElseUnit =  {
      val g = SynthGraph {
        branch
      }
      ElseUnit(pred, g)
    }
  }

  trait LowPri {
    implicit final def Unit[A]: Unit[A] = instance.asInstanceOf[Unit[A]]
    private final val instance = new Unit[Any]
  }
}

sealed trait ElseLike[+A] extends ElseOrElseIfThen[A] {
  def cond: GE = Constant.C1
}

final case class ElseUnit(pred: IfOrElseIfThen[Any], branch: SynthGraph)
  extends ElseLike[Any] {

  def result: Any = ()
}

final case class ElseGE(pred: IfOrElseIfThen[GE], branch: SynthGraph)(val result: GE)
  extends ElseLike[GE] with GE /* .Lazy */ with AudioRated {

  private[synth] def expand: UGenInLike = {
    val b = UGenGraph.builder
    b.visit(ref, sys.error("Trying to expand ElseGE in same nesting level"))
  }
//    UGenGraph.builder match {
//      case sysson: NestedUGenGraphBuilder => sysson.expandIfResult(this, ref)
//      case _ => sys.error("Cannot expand ElseGE outside of NestedUGenGraphBuilder")
//    }
}

final case class ThisBranch() extends GE.Lazy with ControlRated {
  protected def makeUGens: UGenInLike =
    UGenGraph.builder match {
      case sysson: NestedUGenGraphBuilder => sysson.thisBranch
      case _ => sys.error("Cannot expand ThisBranch outside of NestedUGenGraphBuilder")
    }
}