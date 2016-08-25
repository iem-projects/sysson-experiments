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

package de.sciss.synth.ugen.impl.modular

import at.iem.sysson.experiments.{ElseBuilder, ElseIfBuilder, If, IfBuilder}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, UnaryOpUGen}
import de.sciss.synth.{GE, Lazy, MaybeRate, SynthGraph, SysSonUGenGraphBuilder, UGen, UGenGraph, UGenIn, UGenInLike, UndefinedRate, audio, ugen}

import scala.Predef.{any2stringadd => _, _}

object IfBuilderImpl {
  def apply(cond: GE): IfBuilder = new IfBuilderImpl(cond)

  def mkCase[A](cond: GE, branch: => A): IfCase[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    val c = new IfCase[A](cond, g)(res)
    c
  }
}

final class IfBuilderImpl(cond: GE) extends IfBuilder {
  override def toString = s"If (...)@${hashCode().toHexString}"

  def Then[A](branch: => A): If[A] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    IfImpl(c :: Nil)
  }
}

final class ElseIfBuilderImpl[A](cases: List[IfCase[A]], cond: GE) extends ElseIfBuilder[A] {
  override def toString = s"If (...) Then ... ElseIf (...)@${hashCode().toHexString}"

  def Then[B >: A](branch: => B): If[B] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    new IfImpl[B](cases :+ c)
  }
}

trait IfImplLike[A] extends If[A] {
  override def toString = s"If (...) Then ... @${hashCode().toHexString}"

  protected def cases: List[IfCase[A]]

  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilder.Result[B, Out]): Out = {
    val c = IfBuilderImpl.mkCase(1, branch) // XXX TODO --- cheesy way of a `true always` condition?
    result match {
      case ElseBuilder.GE =>
        IfGEImpl((cases :+ c).asInstanceOf[List[IfCase[GE]]]) // XXX TODO --- how to remove the cast?

      case _: ElseBuilder.Unit[_] =>
        IfUnitImpl(cases :+ c)
        ()
    }
  }

  def ElseIf(cond: GE): ElseIfBuilder[A] = new ElseIfBuilderImpl(cases, cond)
}

final case class IfImpl[A](cases: List[IfCase[A]]) extends IfImplLike[A]

final case class IfCase[+A](cond: GE, branch: SynthGraph)(val res: A)

final case class IfUnitImpl(cases: List[IfCase[Any]]) extends Lazy.Expander[Unit] {
  def rate: MaybeRate = UndefinedRate // XXX TODO -- ok?

  protected def makeUGens: Unit = ???
}

final case class IfGEImpl(cases: List[IfCase[GE]]) extends GE.Lazy {
  // integer numbers can be represented as 32-bit floats without loss up to 2^24 - 1
  require(cases.size < 24, s"IfGE -- number of branches cannot be >= 24 (${cases.size})")

  def rate: MaybeRate = MaybeRate.max_?(cases.map(_.res.rate): _*)

  protected def makeUGens: UGenInLike = UGenGraph.builder match {
    case sysson: SysSonUGenGraphBuilder =>
      // makeUGens(sysson)
      sysson.expandIfGE(cases)
    case _ => sys.error(s"Cannot expand modular IfGE outside of SysSon UGen graph builder")
  }

//  private def makeUGens(sysson: SysSonUGenGraphBuilder): UGenInLike = {
//    val id = sysson.allocSubGraph()
//
//    val (_, res) = ((0: GE, 0: GE) /: cases) { case ((condAcc, resAcc), c) =>
//      val bg = c.branch
//      sysson.nested {
//        bg.sources.foreach { lz =>
//          lz.force(sysson)
//        }
//        val resOuts0 = c.res.expand.flatOutputs // XXX TODO --- should we allow bubbling here?
//        val resOuts  = ugen.matchRateFrom(resOuts0, 0, audio)
//        val bus      = ??? : UGenIn
//        UGen.ZeroOut("Out", audio, bus +: resOuts, isIndividual = true)
//      }
//      val condNow: GE = condAcc match {
//        case Constant(0)  => c.cond
//        case cPrev        => UnaryOpUGen.Not.make(cPrev) & c.cond
//      }
//      val condNext: GE = condAcc match {
//        case Constant(0)  => c.cond
//        case cPrev        => cPrev | c.cond
//      }
//      val resNow = c.res * condNow
//      val resNext: GE = resAcc match {
//        case Constant(0)  => resNow
//        case resPrev      => resPrev + resNow
//      }
//      (condNext, resNext)
//    }
//    res
//  }
}