/*
 *  SysSonUGenGraphBuilder.scala
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

package de.sciss.synth

import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.ugen.impl.modular.IfCase
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, UnaryOpUGen}

import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object SysSonUGenGraphBuilder extends UGenGraph.BuilderFactory {
  def build(graph: SynthGraph): UGenGraph = {
    val b = new OuterImpl(graph)
    UGenGraph.use(b) {
      // val proxies = DefaultUGenGraphBuilderFactory.buildWith(graph, b)
      b.build
    }
  }

  private final class InnerImpl extends SysSonUGenGraphBuilder {
    def expandIfGE(cases: List[IfCase[GE]]): GE = ???

    override def visit[U](ref: AnyRef, init: => U): U = ???
  }

  private final class OuterImpl(graph: SynthGraph) extends SysSonUGenGraphBuilder {
    builder =>

    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"

    def build: UGenGraph = {
      ???
      buildGraph(graph)
    }

    private def buildGraph(g0: SynthGraph): UGenGraph = {
      var g = g0
      var controlProxies = ISet.empty[ControlProxyLike]
      while (g.nonEmpty) {
        // XXX these two lines could be more efficient eventually -- using a 'clearable' SynthGraph
        controlProxies ++= g.controlProxies
        g = SynthGraph(g.sources.foreach(_.force(builder))) // allow for further graphs being created
      }
      build(controlProxies)
    }

//    private[this] var _level = 0
//
//    def level: Int = _level
//    def level_=(value: Int): Unit = if (_level != value) {
//        ...
//      _level = value
//    }

    override def visit[U](ref: AnyRef, init: => U): U = ???

//    def allocSubGraph(): Int = ...
//
//    def nested[A](fun: => A): A = ...

    private[this] var ifCount = 0


    private def isBinary(in: GE): Boolean = {
      import BinaryOpUGen._
      in match {
        case Constant(c) => c == 0 || c == 1
        case BinaryOpUGen(op, a, b) =>
          val opi = op.id
          // if (op == Eq || op == Neq || op == Lt || op == Gt || op == Leq || op == Geq) true
          if (opi >= Eq.id && opi <= Geq.id) true
          // else if (op == BitAnd || op == BitOr || op == BitXor) isBinary(a) && isBinary(b)
          else if (opi >= BitAnd.id && opi <= BitXor.id) isBinary(a) && isBinary(b)
          else false

        case UnaryOpUGen(UnaryOpUGen.Not, _) => true
        case _ => false
      }
    }

    private def forceBinary(in: GE): GE = if (isBinary(in)) in else in sig_!= 0

    def expandIfGE(cases: List[IfCase[GE]]): GE = {
      val ifId = ifCount
      ifCount += 1
      cases.zipWithIndex.foreach { case (c, ci) =>
        val condB = forceBinary(c.cond)
        val condS = if (ci == 0) condB else condB sig_== (1 << ci)
        import ugen._
        val nodeCtl = s"$$if${ifId}_$ci"    // e.g. first if block third branch is `$if0_2`
        Pause.kr(gate = condB, node = nodeCtl.ir)
        val graphB = SynthGraph {
          val resBus = s"$$if${ifId}r".ir
          Out.ar(resBus, c.res)
        }
        // now call `UGenGraph.use()` with a child builder, and expand
        // both `c.branch` and `graphB`.

        ???
      }
      ???
    }

    // ---- proxy ----

//    private[this] val current: SysSonUGenGraphBuilder = ...
//
//    def addUGen    (ugen: UGen): Unit = current.addUGen    (ugen)
//    def prependUGen(ugen: UGen): Unit = current.prependUGen(ugen)
//
//    def addControl(values: Vec[Float], name: Option[String]): Int = current.addControl(values, name)
  }
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {
  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

  def expandIfGE(cases: List[IfCase[GE]]): GE

//  var level: Int

//  def allocSubGraph(): Int
//
//  def nested[A](fun: => A): A
}