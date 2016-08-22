package at.iem.sysson.experiments
package impl

import de.sciss.synth.{GE, Lazy, MaybeRate, SynthGraph, UGenInLike}

object IfBuilderImpl {
  def apply(cond: /* => */ GE): IfBuilder = new IfBuilderImpl(/* () => */ cond)

  def mkCase[A](cond: GE, branch: => A): IfCase[A] = {
    var res: A = null.asInstanceOf[A]
    val g = SynthGraph {
      res = branch
    }
    val c = new IfCase[A](cond, g)(res)
    c
  }
}

final class IfBuilderImpl(cond: /* () => */ GE) extends IfBuilder {
  override def toString = s"If (...)@${hashCode().toHexString}"

  def Then[A](branch: => A): If[A] = {
    val c = IfBuilderImpl.mkCase(cond, branch)
    new IfImpl(c :: Nil)
  }
}

final class ElseIfBuilderImpl[A](cases: List[IfCase[A]], cond: /* () => */ GE) extends ElseIfBuilder[A] {
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

  def ElseIf(cond: /* => */ GE): ElseIfBuilder[A] = new ElseIfBuilderImpl(cases, /* () => */ cond)
}

final class IfImpl[A](protected val cases: List[IfCase[A]]) extends IfImplLike[A]

//final class IfCase[+A](val cond: () => GE, branch: () => A)

final case class IfCase[+A](cond: /* () => */ GE, branch: SynthGraph /* () => A */)(val re: A)

final case class IfUnitImpl(cases: List[IfCase[Any]]) extends /* with IfGE */ Lazy.Expander[Unit] {
  def rate: MaybeRate = ???

  protected def makeUGens: Unit = ???
}

final case class IfGEImpl(cases: List[IfCase[GE]]) extends GE.Lazy {
  def rate: MaybeRate = ???

  protected def makeUGens: UGenInLike = ???
}