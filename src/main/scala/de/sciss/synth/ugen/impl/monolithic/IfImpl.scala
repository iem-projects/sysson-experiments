/*
 *  IfImpl.scala
 *  (SysSon-Experiments)
 *
 *  Copyright (c) 2016 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.synth.ugen.impl.monolithic

import at.iem.sysson.experiments.{ElseBuilderT, ElseIfBuilderT, IfBuilderT, IfT}
import de.sciss.synth.ugen.{Constant, UnaryOpUGen}
import de.sciss.synth.{GE, Lazy, MaybeRate, SynthGraph, SysSonUGenGraphBuilder, UGenGraph, UGenInLike, UndefinedRate}

import scala.Predef.{any2stringadd => _, _}

object IfBuilderImpl {
  def apply(cond: GE, lagTime: GE): IfBuilderT = new IfBuilderImpl(cond = cond, lagTime = lagTime)

  def mkCase[A](cond: GE, branch: => A): IfCase[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    val c = new IfCase[A](cond, g)(res)
    c
  }
}

final class IfBuilderImpl(cond: GE, lagTime: GE) extends IfBuilderT {
  override def toString = s"If (...)@${hashCode().toHexString}"

  def Then[A](branch: => A): IfT[A] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    IfImpl(c :: Nil, lagTime = lagTime)
  }
}

final class ElseIfBuilderImpl[A](cases: List[IfCase[A]], cond: GE, lagTime: GE) extends ElseIfBuilderT[A] {
  override def toString = s"If (...) Then ... ElseIf (...)@${hashCode().toHexString}"

  def Then[B >: A](branch: => B): IfT[B] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    new IfImpl[B](cases :+ c, lagTime = lagTime)
  }
}

trait IfImplLike[A] extends IfT[A] {
  override def toString = s"If (...) Then ... @${hashCode().toHexString}"

  protected def cases: List[IfCase[A]]
  protected def lagTime: GE

  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilderT.Result[B, Out]): Out = {
    val c = IfBuilderImpl.mkCase(1, branch) // XXX TODO --- cheesy way of a `true always` condition?
    result match {
      case ElseBuilderT.GE =>
        IfGEImpl((cases :+ c).asInstanceOf[List[IfCase[GE]]], lagTime = lagTime) // XXX TODO --- how to remove the cast?

      case _: ElseBuilderT.Unit[_] =>
        IfUnitImpl(cases :+ c)
        ()
    }
  }

  def ElseIf(cond: GE): ElseIfBuilderT[A] = new ElseIfBuilderImpl(cases, cond = cond, lagTime = lagTime)
}

final case class IfImpl[A](cases: List[IfCase[A]], lagTime: GE) extends IfImplLike[A]

final case class IfCase[+A](cond: GE, branch: SynthGraph)(val res: A)

final case class IfUnitImpl(cases: List[IfCase[Any]]) extends Lazy.Expander[Unit] {
  def rate: MaybeRate = UndefinedRate // XXX TODO -- ok?

  protected def makeUGens: Unit = ???
}

final case class IfGEImpl(cases: List[IfCase[GE]], lagTime: GE) extends GE.Lazy {
  def rate: MaybeRate = MaybeRate.max_?(cases.map(_.res.rate): _*)

  protected def makeUGens: UGenInLike = {
    if (lagTime != Constant.C0) ???

    val (_, res) = ((0: GE, 0: GE) /: cases) { case ((condAcc, resAcc), c) =>
      val condNow: GE = condAcc match {
        case Constant.C0  => c.cond
        case cPrev        => UnaryOpUGen.Not.make(cPrev) & c.cond
      }
      val condNext: GE = condAcc match {
        case Constant.C0  => c.cond
        case cPrev        => cPrev | c.cond
      }
      SysSonUGenGraphBuilder.enterIfCase(condNow)

      val bg = c.branch
      bg.sources.foreach { lz =>
        lz.force(UGenGraph.builder)
      }

      val resNow = c.res * condNow
      val resNext: GE = resAcc match {
        case Constant(0)  => resNow
        case resPrev      => resPrev + resNow
      }
      (condNext, resNext)
    }
    res
  }
}