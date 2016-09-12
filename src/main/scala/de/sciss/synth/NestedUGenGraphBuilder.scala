/*
 *  NestedUGenGraphBuilder.scala
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

import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.ugen.impl.BranchOut
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxyLike, Delay1, ElseGE, ElseOrElseIfThen, If, IfThenLike, Impulse, In, Out, Then, UnaryOpUGen}

import scala.annotation.{elidable, tailrec}
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object NestedUGenGraphBuilder {
  final case class Link(id: Int, rate: Rate, numChannels: Int) {
    require(rate == control || rate == audio, s"Unsupported link rate $rate")
  }

  final class ExpIfTop(val lagTime: GE, val selBranchId: Int) {
    val hasLag      = lagTime != Constant.C0
    var branchCount = 0
    var branches    = List.empty[ExpIfCase]
    var condAcc: GE = Constant.C0

    var resultLinkId  = -1  // summed branch audio output will be written here
    var numChannels   = 0   // of the result signal
  }

  final class ExpIfCase(val peer: Then[Any], val pred: Option[ExpIfCase], val top: ExpIfTop,
                        val predMask: Int, val branchIdx: Int) {
    var resultUsed = false

    val thisMask = predMask | (1 << branchIdx)
  }

  def get: NestedUGenGraphBuilder = UGenGraph.builder match {
    case b: NestedUGenGraphBuilder => b
    case _ => sys.error("Cannot expand modular If-block outside of NestedUGenGraphBuilder")
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

  private def expandLinkSink(link: Link): UGenInLike = {
    val ctlName   = linkCtlName(link.id)
    val ctl       = ctlName.ir    // link-bus
    val in        = In(link.rate, bus = ctl, numChannels = link.numChannels)
    val inExp = in.expand
    // (link, inExp)
    inExp
  }

  private final case class ResultImpl(id: Int, graph: UGenGraph, links: List[Link],
                                      children: List[Result]) extends Result

  /*
    TODO:

    - handle controls over boundaries

   */
  private trait Impl extends NestedUGenGraphBuilder with SynthGraph.Builder {
    builder =>

    // ---- abstract ----

    def outer: OuterImpl
    def parent: Impl
    def childId: Int

    /** Corresponding if-case or `None` if not part of an if-block. */
    def thisExpIfCase: Option[ExpIfCase]

    // ---- impl ----

    protected final var _children = List.empty[Result]    // "reverse-sorted"
    protected final var _links    = List.empty[Link]      // "reverse-sorted"
    private[this] var expIfTops   = List.empty[ExpIfTop]  // "reverse-sorted"; could be a Set but prefer determinism

    private[this] var sources         = Vec.empty[Lazy]
    private[this] var controlProxies  = ISet.empty[ControlProxyLike]

    override def toString = s"SynthGraph.Builder@${hashCode.toHexString}"

    private def errorOutsideBranch(): Nothing =
      throw new UnsupportedOperationException("ThisBranch used outside of if-branch")

    def thisBranch: GE =
      if (If.monolithic) sourceMap.getOrElse("if-case", errorOutsideBranch()).asInstanceOf[GE]
      else {
        val c     = thisExpIfCase                .getOrElse(errorOutsideBranch())
        val in0   = parent.tryRefer("sel-branch").getOrElse(errorOutsideBranch())
        val top   = c.top
        if (top.hasLag) {
          // we don't know if we are the last branch, so simply skip that optimization
          import c.branchIdx
          val lastBranchIdx = top.branchCount - 1
          val condMask = if (branchIdx == lastBranchIdx) in0 else in0 & c.thisMask
          if (branchIdx == 0) condMask else condMask sig_== (1 << branchIdx)
        } else {
          in0
        }
      }

    def enterIfCase(cond: GE): Unit = if (If.monolithic) sourceMap += "if-case" -> cond

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
          // if we just expanded an if-block, we must now
          // wrap the remaining sources in a new child, because
          // only that way we can correctly establish a link
          // between the if-block's return signal and its
          // dependents.
          source match {
            case _: ElseGE =>
              // XXX TODO --- what to do with the damn control proxies?
              val graphC    = SynthGraph(sources = _sources.drop(i + 1), controlProxies = ctlProxiesCpy)
              val child     = new InnerImpl(childId = -1, thisExpIfCase = None, parent = builder, name = "continue")
              // WARNING: call `.build` first before prepending to `_children`,
              // because _children might be updated during `.build` now.
              // Thus _not_ `_children = _children :: child.build(graphC)`!
              val childRes  = child.build(graphC)
              _children   ::= childRes
              i = sz    // "skip rest" in the outer graph
            case _ =>
          }
          i += 1
        }
        _sources = sources

        // at the very end, check for deferred if-blocks;
        // those are blocks whose result hasn't been used,
        // i.e. side-effecting only.
        if (_sources.isEmpty && expIfTops.nonEmpty) {
          val xs = expIfTops
          expIfTops = Nil
          xs.reverse.foreach { expTop =>
            expandIfCases(expTop)
          }
          _sources = sources
        }

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

    final def addLazy(g: Lazy): Unit = sources :+= g

    final def addControlProxy(proxy: ControlProxyLike): Unit = controlProxies += proxy

    // ---- internal ----

    private[this] var linkMap = Map.empty[AnyRef, Link]

    final def tryRefer(ref: AnyRef): Option[UGenInLike] =
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
            addLink(ref, res)
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

        case expIfCase: ExpIfCase =>
          // mark this case and all its predecessors as result-used.
          @tailrec def loop(c: ExpIfCase): Unit = if (!c.resultUsed) {
            c.resultUsed = true
            c.pred match {
              case Some(p) => loop(p)
              case _ =>
            }
          }
          loop(expIfCase)

          val expTop = expIfCase.top
          import expTop._
          if (resultLinkId < 0) resultLinkId = outer.allocId()
          run {
            expandIfCases(expTop)
          }
          expIfTops = expIfTops.filterNot(_ == expTop)    // remove from repeated expansion

          assert(numChannels > 0)
          // println(s"expTop.numChannels = $numChannels")
          val link = Link(id = resultLinkId, rate = audio, numChannels = numChannels)
          addLink(ref, link)

          expandLinkSink(link)
      }

    private def addLink(ref: AnyRef, link: Link): Unit = {
      linkMap += ref -> link
      _links ::= link
    }

    // ---- UGenGraph.Builder ----

    def expandIfCase(c: Then[Any]): ExpIfCase = {
      // create a new ExpIfCase
      val res: ExpIfCase = c match {
        case i: IfThenLike[Any] =>
          val expTop = new ExpIfTop(lagTime = i.dur, selBranchId = outer.allocId())
          expIfTops ::= expTop
          new ExpIfCase(peer = i, top = expTop, pred = None, predMask = 0, branchIdx = 0)

        case e: ElseOrElseIfThen[Any] =>
          val expPar = e.pred.visit(this)
          val expTop = expPar.top
          new ExpIfCase(peer = e, top = expTop, pred = Some(expPar),
            predMask = expPar.predMask | (1 << expPar.branchIdx), branchIdx = expTop.branchCount)
      }

      // register it with the top
      val top = res.top
      top.branchCount += 1
      require(top.branchCount < 24, s"If -- number of branches cannot be >= 24")
      top.branches ::= res

      // make sure the condition is zero or one
      val condBin = forceBinary(c.cond)
      top.condAcc = if (res.branchIdx == 0) {
        condBin
      } else {
        // then "bit-shift" it.
        val condShift = condBin << res.branchIdx
        // then collect the bits in a "bit-field"
        top.condAcc | condShift
      }

      res
    }

    private def expandIfCases(expTop: ExpIfTop): Unit = {
      // This is very elegant: The following elements
      // are side-effect free and will thus be removed
      // from the UGen graph, _unless_ a child is asking
      // for this through `tryRefer`, creating the
      // link on demand. We must store in the source-map
      // _before_ we create the children.

      // the signal written to the branch-selector bus
      // depends on whether we use `If` or `IfLag`.
      //
      // - in the former case, each branch sees the same
      //   trigger signal that is an impulse indicating the
      //   branch has changed. since the branch is only
      //   resumed when it is active, each branch can use
      //   that signal directly without any risk of confusion.
      // - in the latter case, each branch will have a
      //   "release" phase in which already a different branch
      //   condition holds. in order to make it possible to
      //   react to that release, we have to generate a gate
      //   signal instead. we send the delayed `condAcc` to
      //   the bus, and each branch then compares that to its
      //   own branch index.
      import expTop.{hasLag => _hasLag, _}

      // Note: Delay1 does not initialize its state with zero,
      // therefore we have to add a zero frequency impulse.
      val condChange = (Delay1.kr(condAcc) sig_!= condAcc) + Impulse.kr(0)

      val (selBranchSig, condAccT) = if (_hasLag) {
        import ugen._
        // freeze lag time at scalar rate, and ensure it is at
        // least `ControlDur`.
        val lagTimeI = lagTime.rate match {
          case `scalar` => lagTime
          case _        => DC.kr(lagTime)
        }
        val lagTimeM    = lagTimeI.max(ControlDur.ir)

        val condChDly   = TDelay    .kr(condChange, lagTimeM  )
        val condChHold  = SetResetFF.kr(condChange, condChDly )
        val heldAcc     = Latch     .kr(condAcc   , condChHold)

        // DelayN starts with zeroed buffer; we simply add the
        // latched un-delayed beginning during the buffer-fill-up.
        val heldDly0    = DelayN.kr(heldAcc, lagTimeM, lagTimeM)
        val heldDly     = heldDly0 + heldAcc * (heldDly0 sig_== 0)
        (heldAcc, heldDly)

      } else {
        (condChange, condAcc)
      }

      // XXX TODO --- should we use different keys, or
      // is this safe because we'll encounter all the
      // corresponding `ThisBranch` instances right here
      // before leaving the method?
      sourceMap += "sel-branch" -> selBranchSig.expand

      // ----

      // numChannels: Will be maximum across all branches.
      // Note that branches with a smaller number
      // of channels do not "wrap-extend" their
      // signal as would be the case normally in
      // ScalaCollider. Instead, they simply do
      // not contribute to the higher channels.
      // We can add the other behaviour later.

      val lastBranchIdx = branchCount - 1

      branches.reverse.foreach { c =>
        import ugen._
        // the branch condition is met when the fields masked up to here equal the shifted single condition
        val condMask  = if (c.branchIdx == lastBranchIdx) condAccT else condAccT & c.thisMask
        val condEq    = if (c.branchIdx == 0)             condMask else condMask sig_== (1 << c.branchIdx)

        val childId = outer.allocId()
        val nodeCtl = pauseNodeCtlName(childId)
        // condEq.poll(4, s"gate $branchIdx")
        Pause.kr(gate = condEq, node = nodeCtl.ir)

        val graphBranch = if (!c.resultUsed) c.peer.branch else {
          val e = c.peer.asInstanceOf[Then[GE]] // XXX TODO --- not pretty the cast
          val graphTail = SynthGraph {
            val resultCtl   = linkCtlName(resultLinkId)
            val resultBus   = resultCtl.ir
            // val resultRate  = audio
            BranchOut(expTop /* resultRate */, resultBus, e.result)
          }
          val graphHead = e.branch
          graphHead.copy(sources = graphHead.sources ++ graphTail.sources,
            controlProxies = graphHead.controlProxies ++ graphTail.controlProxies)
        }

        // now call `UGenGraph.use()` with a child builder, and expand `graphBranch`
        val child   = new InnerImpl(childId = childId, thisExpIfCase = Some(c),
          parent = builder, name = s"inner{if $resultLinkId case ${c.branchIdx}")
        // println(s"BUILDING BRANCH -- old $numChannels")
        val childRes = child.build(graphBranch)
        // println(s"------------------ new $numChannels")
        _children ::= childRes
      }
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

  private final class InnerImpl(val childId: Int, val thisExpIfCase: Option[ExpIfCase],
                                val parent: Impl, name: String)
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
        } { in => // case (link, in) =>
          // log(this, s"...${smartRef(ref)} -> found in parent: $link")
          log(this, s"...${smartRef(ref)} -> found in parent")
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
    def parent: Impl      = this
    def thisExpIfCase     = None: Option[ExpIfCase]

    def childId           = -1

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
    case nb: NestedUGenGraphBuilder => nb.enterIfCase(cond)
    case _ => // ignore
  }
}
trait NestedUGenGraphBuilder extends BasicUGenGraphBuilder {
  import NestedUGenGraphBuilder.ExpIfCase

  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

//  def expandIfCases(cases: List[IfCase[GE]], lagTime: GE): Link

  /** Returns gate that is open when this if branch is selected. */
  def thisBranch: GE

  /** Used by monolithic if-block. */
  def enterIfCase(cond: GE): Unit

  //////////////////////////////////

  def expandIfCase(c: Then[Any]): ExpIfCase

//  def expandIfResult(e: ElseGE, ref: AnyRef): GE
}