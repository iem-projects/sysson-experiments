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

object SysSonUGenGraphBuilder /* extends UGenGraph.BuilderFactory */ {
  trait Result {
    def graph: UGenGraph
    def children: List[Result]
  }

  def build(graph: SynthGraph): Result = {
    val b = new OuterImpl(graph)
    UGenGraph.use(b) {
      // val proxies = DefaultUGenGraphBuilderFactory.buildWith(graph, b)
      b.build
    }
  }

  def pauseNodeCtlName(ifId: Int, caseId: Int): String =
    s"$$if${ifId}_${caseId}n"    // e.g. first if block third branch is `$if0_2n`

  def pauseBusCtlName(ifId: Int, caseId: Int): String =
    s"$$if${ifId}_${caseId}b"

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

  private final case class ResultImpl(graph: UGenGraph, children: List[Result]) extends Result

  private trait Impl extends SysSonUGenGraphBuilder {
    builder =>

    // ---- abstract ----

    def outer: OuterImpl

    protected def graph: SynthGraph

    // ---- impl ----

    protected var _children = List.empty[Result]

    def children: List[Result] = _children.reverse

    final def build: Result = {
      val ugens = buildGraph(graph)
      ResultImpl(ugens, children)
    }

    override def visit[U](ref: AnyRef, init: => U): U = {
      // log(s"visit  ${ref.hashCode.toHexString}")
      sourceMap.getOrElse(ref, {
        // log(s"expand ${ref.hashCode.toHexString}...")
        val exp    = init
        // log(s"...${ref.hashCode.toHexString} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        sourceMap += ref -> exp
        exp
      }).asInstanceOf[U] // not so pretty...
    }

    final def expandIfGE(cases: List[IfCase[GE]]): GE = {
      val ifId = outer.allocIfId()
      var condAcc: GE = 0
      cases.zipWithIndex.foreach { case (c, ci) =>
        // make sure the condition is zero or one
        val condBin   = forceBinary(c.cond)
        // then "bit-shift" it. XXX TODO --- does SuperCollider BinaryOpUGen implement bit-shift?
        val condShift = if (ci == 0) condBin else condBin sig_== (1 << ci)
        // then collect the bits in a "bit-field"
        condAcc       = if (ci == 0) condBin else condAcc | condShift
        // then the branch condition is met when the field equals the shifted single condition
        val condEq    = if (ci == 0) condBin else condAcc sig_== condShift
        import ugen._
        val nodeCtl = pauseNodeCtlName(ifId, ci)
        val busCtl  = pauseBusCtlName (ifId, ci)
        Pause.kr(gate = condEq, node = nodeCtl.ir)
        Out.kr(bus = busCtl.ir, in = condEq)    // child can monitor its own pause state that way
        val graphB = SynthGraph {
          val resBus = s"$$if${ifId}r".ir
          Out.ar(resBus, c.res)
        }
        // now call `UGenGraph.use()` with a child builder, and expand
        // both `c.branch` and `graphB`.
        val graphC = c.branch.copy(sources = c.branch.sources ++ graphB.sources,
          controlProxies = c.branch.controlProxies ++ graphB.controlProxies)
        val child = new InnerImpl(outer, graphC, ifId = ifId, caseId = ci)
        _children ::= UGenGraph.use(child) {
          child.build
        }
      }
      ugen.DC.ar(0) // XXX TODO
    }

    protected final def buildGraph(g0: SynthGraph): UGenGraph = {
      var g = g0
      var controlProxies = ISet.empty[ControlProxyLike]
      while (g.nonEmpty) {
        // XXX these two lines could be more efficient eventually -- using a 'clearable' SynthGraph
        controlProxies ++= g.controlProxies
        g = SynthGraph(g.sources.foreach(_.force(builder))) // allow for further graphs being created
      }
      build(controlProxies)
    }
  }

  private final class InnerImpl(val outer: OuterImpl, val graph: SynthGraph, ifId: Int, caseId: Int)
    extends Impl {

    def allocIfId(): Int = outer.allocIfId()

//    def expandIfGE(cases: List[IfCase[GE]]): GE = {
//      // we would need to get the `ifCount` from parent
//      // and AND with the parent branch cond.
//      // Alternatively, we create a `Group` that will
//      // then be the node-ID for the parent to pause/resume.
//      // That way we can avoid the AND.
//      throw new NotImplementedError("Nested expandIfGE")
//      ...
//    }
  }

  private final class OuterImpl(val graph: SynthGraph) extends Impl {
    builder =>

    def outer: OuterImpl = this

    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"

//    private[this] var _level = 0
//
//    def level: Int = _level
//    def level_=(value: Int): Unit = if (_level != value) {
//        ...
//      _level = value
//    }

//    def allocSubGraph(): Int = ...
//
//    def nested[A](fun: => A): A = ...

    private[this] var ifCount = 0

    def allocIfId(): Int = {
      val res = ifCount
      ifCount += 1
      res
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