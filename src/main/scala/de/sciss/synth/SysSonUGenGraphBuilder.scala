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
import de.sciss.synth.ugen.impl.modular.{IfCase, IfGEImpl, IfRef}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, In, Out, UnaryOpUGen}

import scala.annotation.elidable
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object SysSonUGenGraphBuilder {
  final case class Link(id: Int, numChannels: Int)

  trait Result {
    def graph: UGenGraph

    def linkOut: List[Link]

    // XXX TODO --- since all synths will be started within the same group,
    // we don't actually need to remember the link-in list, because if
    // we set the control on the group both sink and source will see it.
    def linkIn: List[Link]

    def children: List[Result]
  }

  def build(graph: SynthGraph): Result = {
    val b = new OuterImpl
    b.build(graph)
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
    protected var _linkIn   = List.empty[Link]
    protected var _linkOut  = List.empty[Link]

    private[this] var sources         = Vec.empty[Lazy]    // g0.sources
    private[this] var controlProxies  = ISet.empty[ControlProxyLike]  // g0.controlProxies

    override def toString = s"SynthGraph.Builder@${hashCode.toHexString}"

    final def build(g0: SynthGraph): Result = SynthGraph.use(builder) {
      UGenGraph.use(builder) {
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
        ResultImpl(ugenGraph, linkIn = _linkIn.reverse, linkOut = _linkOut.reverse, children = _children.reverse)
      }
    }

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

    final def tryRefer(ref: AnyRef): Option[(Link, UGenInLike)] =
      sourceMap.get(ref).collect {
        case sig: UGenInLike =>
          val link        = linkMap.getOrElse(ref, {
            val numChannels = sig.outputs.size
            val linkId      = outer.allocLinkId()
            val res         = Link(id = linkId, numChannels = numChannels)
            linkMap += ref -> res
            res
          })
          val ctlName     = linkCtlName(link.id)
          // an undefined rate - which we forbid - can only occur with mixed UGenInGroup
          val rate        = sig.rate match {
            case r: Rate => r
            case _ => throw new IllegalArgumentException("Cannot refer to UGen group with mixed rates across branches")
          }
          // Add a control and `Out` to this (parent) graph.
          // This is super tricky -- we have to encapsulate
          // in a new synth graph because otherwise GE will
          // end up in the caller's synth graph; there is
          // no way we can catch them in our own outer synth graph,
          // so we must then force them explicitly!
          run {
            val ctl = ctlName.ir    // link-bus
            Out(rate, bus = ctl, in = sig)
          }
          // then add a control and `In` to the caller (child) graph
          val ctl = ctlName.ir    // link-bus
          val in  = In(rate, bus = ctl, numChannels = link.numChannels)
          _linkOut ::= link
          val inExp = in.expand
          (link, inExp)

        case ref: IfRef => ???
      }

    // ---- UGenGraph.Builder ----

    final def expandIfCases(cases: List[IfCase[GE]]): IfRef = {
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
        val child = new InnerImpl(builder, name = s"inner{if $ifId case $ci}")
        _children ::= child.build(graphC)
      }
      IfRef(ifId)
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
          _linkIn ::= link
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

  var showLog = true

  @elidable(elidable.CONFIG) private def log(builder: Impl, what: => String): Unit =
    if (showLog) println(s"ScalaCollider-DOT <${builder.toString}> $what")
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {
  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

  def expandIfCases(cases: List[IfCase[GE]]): IfRef
}