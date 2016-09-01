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
import de.sciss.synth.ugen.impl.modular.{IfCase, IfGEImpl}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, In, Out, UnaryOpUGen}

import scala.annotation.elidable
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object SysSonUGenGraphBuilder {
  final case class Link(id: Int, rate: Rate, numChannels: Int)

  trait Result {
    def graph: UGenGraph

    /** Outgoing links for this sub-graph. */
    def links: List[Link]

    def children: List[Result]
  }

  def build(graph: SynthGraph): Result = {
    val b = new OuterImpl
    b.build(graph)
  }

  def pauseNodeCtlName(linkId: Int, caseId: Int): String =
    s"$$if${linkId}_${caseId}n"    // e.g. first if block third branch is `$if0_2n`

  def pauseBusCtlName(linkId: Int, caseId: Int): String =
    s"$$if${linkId}_${caseId}b"

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

  private final case class ResultImpl(graph: UGenGraph, /* linkIn: List[Link], */ links: List[Link],
                                      children: List[Result]) extends Result

  /*
    TODO:

    - more efficient bundling of buses (e.g. one control per if block)
    - create return signal
    - handle controls over boundaries

   */
  private trait Impl extends SysSonUGenGraphBuilder with SynthGraph.Builder {
    builder =>

    // ---- abstract ----

    def outer: OuterImpl

    // ---- impl ----

    protected var _children = List.empty[Result]
//    protected var _linkIn   = List.empty[Link]
    protected var _linkOut  = List.empty[Link]

    private[this] var sources         = Vec.empty[Lazy]    // g0.sources
    private[this] var controlProxies  = ISet.empty[ControlProxyLike]  // g0.controlProxies

    override def toString = s"SynthGraph.Builder@${hashCode.toHexString}"

    final def buildInner(g0: SynthGraph): Result = {
      var _sources: Vec[Lazy] = g0.sources
      controlProxies = g0.controlProxies
      do {
        sources = Vector.empty
        val ctlProxiesCpy = controlProxies
        var i = 0
        val sz = _sources.size
        while (i < sz) {
          val source = _sources(i)
          source.force(builder)
          _sources(i) match {
            case _: IfGEImpl =>
              // XXX TODO --- what to do with the damn control proxies?
              val graphC = SynthGraph(sources = _sources.drop(i + 1), controlProxies = ctlProxiesCpy)
              val child  = new InnerImpl(builder, name = "continue")
              _children ::= child.build(graphC)
              i = sz    // "skip rest"
            case _ =>
          }
          i += 1
        }
        _sources = sources
      } while (_sources.nonEmpty)
      val ugenGraph = build(controlProxies)
      ResultImpl(ugenGraph, /* linkIn = _linkIn.reverse, */ links = _linkOut.reverse, children = _children.reverse)
    }

    final def build(g0: SynthGraph): Result = run(buildInner(g0))

    final def run[A](thunk: => A): A = SynthGraph.use(builder) {
      UGenGraph.use(builder) {
        thunk
      }
    }

    // ---- SynthGraph.Builder ----

//    final def addLazy(g: Lazy): Unit = sources += g
    final def addLazy(g: Lazy): Unit = sources :+= g

    final def addControlProxy(proxy: ControlProxyLike): Unit = controlProxies += proxy

    // ---- internal ----

    private[this] var linkMap = Map.empty[AnyRef, Link]

    private def expandLinkSink(link: Link): (Link, UGenInLike) = {
      val ctlName   = linkCtlName(link.id)
      val ctl       = ctlName.ir    // link-bus
      val in        = In(link.rate, bus = ctl, numChannels = link.numChannels)
      val inExp = in.expand
      (link, inExp)
    }

    final def tryRefer(ref: AnyRef): Option[(Link, UGenInLike)] =
      sourceMap.get(ref).collect {
        case sig: UGenInLike =>
          val link        = linkMap.getOrElse(ref, {
            val numChannels = sig.outputs.size
            val linkId      = outer.allocLinkId()
            // an undefined rate - which we forbid - can only occur with mixed UGenInGroup
            val linkRate    = sig.rate match {
              case r: Rate => r
              case _ => throw new IllegalArgumentException("Cannot refer to UGen group with mixed rates across branches")
            }
            val res         = Link(id = linkId, rate = linkRate, numChannels = numChannels)
            linkMap += ref -> res
            _linkOut ::= res
            val ctlName     = linkCtlName(linkId)
            // Add a control and `Out` to this (parent) graph.
            // This is super tricky -- we have to encapsulate
            // in a new synth graph because otherwise GE will
            // end up in the caller's synth graph; there is
            // no way we can catch them in our own outer synth graph,
            // so we must then force them explicitly!
            run {
              val ctl = ctlName.ir    // link-bus
              Out(linkRate, bus = ctl, in = sig)
            }
            res
          })
          expandLinkSink(link)

        case link: Link =>
          // if reference found
          expandLinkSink(link)
      }

    // ---- UGenGraph.Builder ----

    final def expandIfCases(cases: List[IfCase[GE]]): Link = {
      val linkId = outer.allocLinkId()
      var condAcc: GE = 0
      // Will be maximum across all branches.
      // Note that branches with a smaller number
      // of channels do not "wrap-extend" their
      // signal as would be the case normally in
      // ScalaCollider. Instead, they simply do
      // not contribute to the higher channels.
      // We can add the other behaviour later.
      var numChannels = 0
      cases.zipWithIndex.foreach { case (c, ci) =>
        // make sure the condition is zero or one
        val condBin   = forceBinary(c.cond)
        // then "bit-shift" it. XXX TODO --- does BinaryOpUGen needs to implement bit-shift
        // https://github.com/Sciss/ScalaColliderUGens/issues/23
        val condShift = if (ci == 0) condBin else condBin * (1 << ci)
        // then collect the bits in a "bit-field"
        condAcc       = if (ci == 0) condBin else condAcc | condShift
        // then the branch condition is met when the field equals the shifted single condition
        val condEq    = if (ci == 0) condBin else condAcc sig_== condShift
        import ugen._
        val nodeCtl = pauseNodeCtlName(linkId, ci)
        val busCtl  = pauseBusCtlName (linkId, ci)
        Pause.kr(gate = condEq, node = nodeCtl.ir)
        Out.kr(bus = busCtl.ir, in = condEq)    // child can monitor its own pause state that way
        val resCtl = linkCtlName(linkId)
        val graphB = SynthGraph {
          val linkBus   = resCtl.ir
          val linkRate  = audio // XXX TODO --- how to get rate?
          Out(linkRate, linkBus, c.res)
        }
        // now call `UGenGraph.use()` with a child builder, and expand
        // both `c.branch` and `graphB`.
        val graphC = c.branch.copy(sources = c.branch.sources ++ graphB.sources,
          controlProxies = c.branch.controlProxies ++ graphB.controlProxies)
        val child = new InnerImpl(builder, name = s"inner{if $linkId case $ci}")
        val childRes = child.run {
          val res         = child.buildInner(graphC)
          val sig         = c.res.expand
          val childChans  = sig.outputs.size
          numChannels     = math.max(numChannels, childChans)
          res
        }
        _children ::= childRes
      }
      Link(id = linkId, rate = audio, numChannels = numChannels)  // XXX TODO --- how to get rate?
    }
  }

  private def smartRef(ref: AnyRef): String = {
    val t = new Throwable
    t.fillInStackTrace()
    val trace = t.getStackTrace
    val opt = trace.collectFirst {
      case ste if (ste.getMethodName == "force" || ste.getMethodName == "expand") && ste.getFileName != "Lazy.scala" =>
        val clz = ste.getClassName
        val i   = clz.lastIndexOf(".") + 1
        val j   = clz.lastIndexOf("@", i)
        val s   = if (j < 0) clz.substring(i) else clz.substring(i, j)
        s"$s@${ref.hashCode().toHexString}"
    }
    opt.getOrElse(ref.hashCode.toHexString)
  }

  private final class InnerImpl(parent: Impl, name: String)
    extends Impl {

    def outer: OuterImpl = parent.outer

    override def toString = name

    override def visit[U](ref: AnyRef, init: => U): U = visit1[U](ref, () => init)

    private def visit1[U](ref: AnyRef, init: () => U): U = {
      // log(s"visit  ${ref.hashCode.toHexString}")
      sourceMap.getOrElse(ref, {
        log(this, s"expand ${smartRef(ref)}...")
        val exp = parent.tryRefer(ref).fold[Any] {
          log(this, s"...${smartRef(ref)} -> not yet found")
          init()
        } { case (link, in) =>
          log(this, s"...${smartRef(ref)} -> found in parent: $link")
//          _linkIn ::= link
          in
        }
        sourceMap += ref -> exp
        log(this, s"...${smartRef(ref)} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        exp
      }).asInstanceOf[U] // not so pretty...
    }
  }

  private final class OuterImpl extends Impl {
    builder =>

    def outer: OuterImpl = this

//    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"
    override def toString = "outer"

    private[this] var linkCount = 0

    def allocLinkId(): Int = {
      val res = linkCount
      linkCount += 1
      res
    }

    override def visit[U](ref: AnyRef, init: => U): U = {
      log(this, s"visit  ${smartRef(ref)}")
      sourceMap.getOrElse(ref, {
        log(this, s"expand ${smartRef(ref)}...")
        val exp = init
        log(this, s"...${smartRef(ref)} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        sourceMap += ref -> exp
        exp
      }).asInstanceOf[U] // not so pretty...
    }
  }

  private def printSmart(x: Any): String = x match {
    case u: UGen  => u.name
    case _        => x.toString
  }

  var showLog = false

  @elidable(elidable.CONFIG) private def log(builder: Impl, what: => String): Unit =
    if (showLog) println(s"ScalaCollider-DOT <${builder.toString}> $what")
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {
  import SysSonUGenGraphBuilder.Link

  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

  def expandIfCases(cases: List[IfCase[GE]]): Link
}