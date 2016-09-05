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

import at.iem.sysson.experiments.If
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.ugen.impl.modular.{IfCase, IfGEImpl}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, In, Out, UnaryOpUGen}

import scala.annotation.elidable
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object SysSonUGenGraphBuilder {
  final case class Link(id: Int, rate: Rate, numChannels: Int) {
    require(rate == control || rate == audio, s"Unsupported link rate $rate")
  }

  trait Result {
    /** For every child whose `id` is greater than
      * or equal to zero, a control must be set
      * based on `pauseNodeCtlName`.
      */
    def id: Int

    def graph: UGenGraph

    /** Outgoing links.
      * To "play" the result, for each link
      * a corresponding bus must be allocated
      * and set through a control obtained
      * with `linkCtlName(id)`.
      */
    def links: List[Link]

    /** For each child, a synth must be created and nested
      * in the parent group. The synth's id must be
      * communicated through `pauseNodeCtlName`
      */
    def children: List[Result]
  }

  def build(graph: SynthGraph): Result = {
    val b = new OuterImpl
    b.build(graph)
  }

  def pauseNodeCtlName(id: Int): String =
    s"$$if$id" // e.g. first if block third branch is `$if0_2n`

//  def pauseNodeCtlName(linkId: Int, caseId: Int): String =
//    s"$$if${linkId}_${caseId}n"    // e.g. first if block third branch is `$if0_2n`
//
//  def pauseBusCtlName(linkId: Int, caseId: Int): String =
//    s"$$if${linkId}_${caseId}b"

  // single control for setting the bus index
  def linkCtlName(id: Int): String =
    s"$$ln$id"

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

  private final case class ResultImpl(id: Int, graph: UGenGraph, links: List[Link],
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
    def childId: Int

    /** The link identifier for the control-rate bus
      * that carries the selected-branch-signal.
      */
    def selectedBranchId: Int

    /** Index of current branch or if-case in the current if-block. */
    def branchIdx: Int

    // ---- impl ----

    protected final var _children = List.empty[Result]
    protected final var _links    = List.empty[Link]

    private[this] var sources         = Vec.empty[Lazy]    // g0.sources
    private[this] var controlProxies  = ISet.empty[ControlProxyLike]  // g0.controlProxies

    override def toString = s"SynthGraph.Builder@${hashCode.toHexString}"

    private[this] var enteredIfCase = Option.empty[GE]

    private def errorOutsideBranch(): Nothing =
      throw new UnsupportedOperationException("ThisBranch used outside of if-branch")

    def thisBranch: GE =
      if (If.monolithic) enteredIfCase.getOrElse(errorOutsideBranch())
      else {
        // the branch is selected if the "bit" for this
        // branch is set, and the bits for all lower branches
        // are clear. E.g. for the third branch: x & ((1 << 3) - 1) == (1 << 2)
        // I.e. x & 7 == 4
        val selBranchId = selectedBranchId
        if (selBranchId < 0) errorOutsideBranch()
        val selCtlName  = linkCtlName(selBranchId)
        val selBus      = selCtlName.ir
        val condAcc = In.kr(selBus)
        println(s"cond-in-ctl $selCtlName")
        condAcc.poll(4, "cond-in")
        selBus .poll(4, "cond-in-bus")
        condAcc & ((1 << (branchIdx + 1)) - 1) sig_== (1 << branchIdx)
      }

    def enterIfCase(cond: GE): Unit = if (If.monolithic) enteredIfCase = Some(cond)

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
              val graphC  = SynthGraph(sources = _sources.drop(i + 1), controlProxies = ctlProxiesCpy)
              val child   = new InnerImpl(childId = -1, selectedBranchId = -1, branchIdx = -1,
                parent = builder, name = "continue")
              _children ::= child.build(graphC)
              i = sz    // "skip rest"
            case _ =>
          }
          i += 1
        }
        _sources = sources
      } while (_sources.nonEmpty)
      val ugenGraph = build(controlProxies)
      ResultImpl(id = childId, graph = ugenGraph, links = _links.reverse, children = _children.reverse)
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
            val linkId      = outer.allocId()
            // an undefined rate - which we forbid - can only occur with mixed UGenInGroup
            val linkRate    = sig.rate match {
              case `scalar` => control
              case r: Rate  => r
              case _ => throw new IllegalArgumentException("Cannot refer to UGen group with mixed rates across branches")
            }
            val res         = Link(id = linkId, rate = linkRate, numChannels = numChannels)
            linkMap += ref -> res
            _links ::= res
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
      val resultLinkId = outer.allocId()
      val selBranchId  = outer.allocId()
      var condAcc: GE = 0
      // Will be maximum across all branches.
      // Note that branches with a smaller number
      // of channels do not "wrap-extend" their
      // signal as would be the case normally in
      // ScalaCollider. Instead, they simply do
      // not contribute to the higher channels.
      // We can add the other behaviour later.
      var numChannels = 0
      val lastBranchIdx = cases.size - 1
      cases.zipWithIndex.foreach { case (c, branchIdx) =>
        // make sure the condition is zero or one
        val condBin = forceBinary(c.cond)
        val condEq = if (branchIdx == 0) {
          condAcc = condBin
          condBin
        } else {
          // then "bit-shift" it.
          val condShift = condBin << branchIdx
          // then collect the bits in a "bit-field"
          condAcc |= condShift
          // then the branch condition is met when the fields masked up to here equal the shifted single condition
          val condMask = if (branchIdx == lastBranchIdx) condAcc else condAcc & ((1 << (branchIdx + 1)) - 1)
          condMask sig_== (1 << branchIdx)
        }
        import ugen._
        val childId = outer.allocId()
        val nodeCtl = pauseNodeCtlName(childId)
        // condEq.poll(4, s"gate $branchIdx")
        Pause.kr(gate = condEq, node = nodeCtl.ir)
        val resultCtl = linkCtlName(resultLinkId)
        val graphB = SynthGraph {
          val resultBus   = resultCtl.ir
          val resultRate  = audio // XXX TODO --- how to get rate?
          Out(resultRate, resultBus, c.res)
        }
        // now call `UGenGraph.use()` with a child builder, and expand
        // both `c.branch` and `graphB`.
        val graphC = c.branch.copy(sources = c.branch.sources ++ graphB.sources,
          controlProxies = c.branch.controlProxies ++ graphB.controlProxies)
        val child   = new InnerImpl(childId = childId, selectedBranchId = selBranchId,
          branchIdx = branchIdx, parent = builder, name = s"inner{if $resultLinkId case $branchIdx}")
        val childRes = child.run {
          val res         = child.buildInner(graphC)
          val sig         = c.res.expand
          val childChans  = sig.outputs.size
          numChannels     = math.max(numChannels, childChans)
          res
        }
        _children ::= childRes
      }

      val linkSelBranch = Link(id = selBranchId, rate = control, numChannels = 1)
      _links ::= linkSelBranch
      val selCtlName  = linkCtlName(selBranchId)
      val selBus      = selCtlName.ir
      println(s"cond-out-ctl $selCtlName")
      condAcc.poll(4, "cond-out")
      selBus .poll(4, "cond-out-bus")
      Out.kr(bus = selBus, in = condAcc)

      val linkRes = Link(id = resultLinkId, rate = audio, numChannels = numChannels)  // XXX TODO --- how to get rate?
      _links ::= linkRes
      linkRes
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

  private final class InnerImpl(val childId: Int, val selectedBranchId: Int, val branchIdx: Int,
                                parent: Impl, name: String)
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

    def outer: OuterImpl  = this
    def childId           = -1
    def selectedBranchId  = -1
    def branchIdx         = -1

    //    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"
    override def toString = "outer"

    private[this] var idCount = 0

    /** Allocates a unique increasing identifier. */
    def allocId(): Int = {
      val res = idCount
      idCount += 1
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

  def enterIfCase(cond: GE): Unit = UGenGraph.builder match {
    case sysson: SysSonUGenGraphBuilder => sysson.enterIfCase(cond)
    case _ => // ignore
  }
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {
  import SysSonUGenGraphBuilder.Link

  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

  def expandIfCases(cases: List[IfCase[GE]]): Link

  /** Returns gate that is open when this if branch is selected. */
  def thisBranch: GE

  /** Used by monolithic if-block. */
  def enterIfCase(cond: GE): Unit
}