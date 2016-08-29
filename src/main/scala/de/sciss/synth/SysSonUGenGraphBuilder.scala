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
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, In, Out, UnaryOpUGen}

import scala.annotation.elidable
import scala.collection.immutable.{Set => ISet}

object SysSonUGenGraphBuilder /* extends UGenGraph.BuilderFactory */ {
  final case class Link(id: Int, numChannels: Int)

  trait Result {
    def graph: UGenGraph

    def linkOut  : List[Link]
    def linkIn   : List[Link]

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

  def linkCtlName(linkId: Int): String =
    s"$$lnk$linkId"

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

  private final case class ResultImpl(graph: UGenGraph, linkIn: List[Link], linkOut: List[Link],
                                      children: List[Result]) extends Result

  /*
    TODO:

    - find expanded source in parents and establish link
    - more efficient bundling of buses (e.g. one control per if block)
    - create return signal

   */
  private trait Impl extends SysSonUGenGraphBuilder {
    builder =>

    // ---- abstract ----

    def outer: OuterImpl

    protected def graph: SynthGraph

    // ---- impl ----

    protected var _children = List.empty[Result]
    protected var _linkIn   = List.empty[Link]
    protected var _linkOut  = List.empty[Link]

    // def children: List[Result] = _children.reverse

    final def build: Result = {
      val ugens = buildGraph(graph)
      ResultImpl(ugens, linkIn = _linkIn.reverse, linkOut = _linkOut.reverse, children = _children.reverse)
    }

    final def tryRefer[U](ref: AnyRef, init: => U): Option[(Link, U)] =
      sourceMap.get(ref).collect {
        case sig: UGenInLike =>
          val numChannels = sig.outputs.size
          val linkId      = outer.allocLinkId()
          val link        = Link(id = linkId, numChannels = numChannels)
          val ctlName     = linkCtlName(linkId)
          // an undefined rate - which we forbid - can only occur with mixed UGenInGroup
          val rate        = sig.rate match {
            case r: Rate => r
            case _ => throw new IllegalArgumentException("Cannot refer to UGen group with mixed rates across branches")
          }
          // add a control and `Out` to this (parent) graph
          UGenGraph.use(builder) {
            val ctl = ctlName.ir    // link-bus
            Out(rate, bus = ctl, in = sig)
          }
          // then add a control and `In` to the caller (child) graph
          val ctl = ctlName.ir    // link-bus
          val in  = In(rate, bus = ctl, numChannels = numChannels)
          _linkOut ::= link
          (link, in.expand.asInstanceOf[U])
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
        val child = new InnerImpl(builder, graphC, ifId = ifId, caseId = ci)
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

  private final class InnerImpl(parent: Impl, val graph: SynthGraph, ifId: Int, caseId: Int)
    extends Impl {

    def outer: OuterImpl = parent.outer

    override def toString = s"inner{if $ifId case $caseId}"

    override def visit[U](ref: AnyRef, init: => U): U = {
      // log(s"visit  ${ref.hashCode.toHexString}")
      sourceMap.getOrElse(ref, {
        log(this, s"expand ${ref.hashCode.toHexString}...")
        val exp = parent.tryRefer(ref, init).fold {
          log(this, s"...${ref.hashCode.toHexString} -> not yet found")
          init
        } { case (link, in) =>
          log(this, s"...${ref.hashCode.toHexString} -> found in parent: $link")
          _linkIn ::= link
          in
        }
        sourceMap += ref -> exp
        log(this, s"...${ref.hashCode.toHexString} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        exp
      }).asInstanceOf[U] // not so pretty...
    }

    //    def allocIfId(): Int = outer.allocIfId()

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

//    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"
    override def toString = "outer"

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

    private[this] var linkCount = 0

    def allocLinkId(): Int = {
      val res = linkCount
      linkCount += 1
      res
    }

    override def visit[U](ref: AnyRef, init: => U): U = {
      log(this, s"visit  ${ref.hashCode.toHexString}")
      sourceMap.getOrElse(ref, {
        log(this, s"expand ${ref.hashCode.toHexString}...")
        val exp = init
        log(this, s"...${ref.hashCode.toHexString} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        sourceMap += ref -> exp
        exp
      }).asInstanceOf[U] // not so pretty...
    }
  }

  private def printSmart(x: Any): String = x match {
    case u: UGen  => u.name
    case _        => x.toString
  }

  var showLog = true

  @elidable(elidable.CONFIG) private def log(builder: Impl, what: => String): Unit =
    if (showLog) println(s"ScalaCollider-DOT <${builder.toString}> $what")
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {
  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

  def expandIfGE(cases: List[IfCase[GE]]): GE

//  var level: Int

//  def allocSubGraph(): Int
//
//  def nested[A](fun: => A): A
}