/*
 *  IfImpl.scala
 *  (SysSon-Experiments)
 *
 *  Copyright (c) 2016 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.synth.ugen.impl.modular

import at.iem.sysson.experiments.old.{ElseBuilderOLD, ElseIfBuilderOLD, IfBuilderOLD, IfOLD}
import de.sciss.synth.UGenGraph.Builder
import de.sciss.synth.{GE, Lazy, MaybeRate, SynthGraph, SysSonUGenGraphBuilderOLD, UGenGraph, UGenInLike, UndefinedRate}

import scala.Predef.{any2stringadd => _, _}

object IfBuilderImpl {
  /** Use `Constant.C0` for no lag. */
  def apply(cond: GE, lagTime: GE): IfBuilderOLD = new IfBuilderImpl(cond, lagTime = lagTime)

  def mkCase[A](cond: GE, branch: => A): IfCase[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    val c = new IfCase[A](cond, g)(res)
    c
  }
}

final class IfBuilderImpl(cond: GE, lagTime: GE) extends IfBuilderOLD {
  override def toString = s"If (...)@${hashCode().toHexString}"

  def Then[A](branch: => A): IfOLD[A] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    IfImpl(c :: Nil, lagTime = lagTime)
  }
}

final class ElseIfBuilderImpl[A](cases: List[IfCase[A]], cond: GE, lagTime: GE) extends ElseIfBuilderOLD[A] {
  override def toString = s"If (...) Then ... ElseIf (...)@${hashCode().toHexString}"

  def Then[B >: A](branch: => B): IfOLD[B] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    new IfImpl[B](cases :+ c, lagTime = lagTime)
  }
}

trait IfImplLike[A] extends IfOLD[A] {
  override def toString = s"If (...) Then ... @${hashCode().toHexString}"

  protected def cases: List[IfCase[A]]
  protected def lagTime: GE

  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilderOLD.Result[B, Out]): Out = {
    val c = IfBuilderImpl.mkCase(1, branch) // XXX TODO --- cheesy way of a `true always` condition?
    result match {
      case ElseBuilderOLD.GE =>
        // XXX TODO --- how to remove the cast?
        IfGEImpl((cases :+ c).asInstanceOf[List[IfCase[GE]]], lagTime = lagTime)

      case _: ElseBuilderOLD.Unit[_] =>
        IfUnitImpl(cases :+ c)
        ()
    }
  }

  def ElseIf(cond: GE): ElseIfBuilderOLD[A] = new ElseIfBuilderImpl(cases, cond = cond, lagTime = lagTime)
}

final case class IfImpl[A](cases: List[IfCase[A]], lagTime: GE) extends IfImplLike[A]

final case class IfCase[+A](cond: GE, branch: SynthGraph)(val res: A)

final case class IfUnitImpl(cases: List[IfCase[Any]]) extends Lazy.Expander[Unit] {
  def rate: MaybeRate = UndefinedRate // XXX TODO -- ok?

  protected def makeUGens: Unit = ???
}

final case class IfGEImpl(cases: List[IfCase[GE]], lagTime: GE) extends GE with Lazy {
  // integer numbers can be represented as 32-bit floats without loss up to 2^24 - 1
  require(cases.size < 24, s"IfGE -- number of branches cannot be >= 24 (${cases.size})")

  // same as `Lazy.Expander`, but we don't use its implementation of `expand`
  @transient private[this] lazy val ref = new AnyRef

  // ---- constructor ----
  SynthGraph.builder.addLazy(this)

  def rate: MaybeRate = MaybeRate.max_?(cases.map(_.res.rate): _*)

  private def failNoForce(): Nothing =
    throw new IllegalStateException("IfGE - encountering `expand` without prior `force`")

  private[synth] def expand: UGenInLike =
    UGenGraph.builder.visit[UGenInLike](ref, failNoForce())

  private[synth] def expandAny: Any =
    UGenGraph.builder.visit[Any](ref, failNoForce())

  // the `expandIfCases` will store the reference!
  private[synth] def force(b: Builder): Unit = UGenGraph.builder match {
    case sysson: SysSonUGenGraphBuilderOLD =>
      sysson.visit(ref, sysson.expandIfCases(cases, lagTime))

    case _ => sys.error(s"Cannot expand modular IfGE outside of SysSon UGen graph builder")
  }
}