package at.iem.sysson.experiments
package impl

import de.sciss.synth.{GE, MaybeRate, UGenInLike}

object IfBuilderImpl {
  def apply(cond: => GE): IfBuilder = new IfBuilderImpl(() => cond)
}

final class IfBuilderImpl(cond: () => GE) extends IfBuilder {
  override def toString = s"If (...)@${hashCode().toHexString}"

  def Then[A](branch: => A): If[A] = new IfImpl(new IfCase(cond, () => branch) :: Nil)
}

final class ElseIfBuilderImpl[A](cases: List[IfCase[A]], cond: () => GE) extends ElseIfBuilder[A] {
  override def toString = s"If (...) Then ... ElseIf (...)@${hashCode().toHexString}"

  def Then[B >: A](branch: => B): If[B] = new IfImpl[B](cases :+ new IfCase(cond, () => branch))
}

trait IfImplLike[A] extends If[A] {
  override def toString = s"If (...) Then ... @${hashCode().toHexString}"

  protected def cases: List[IfCase[A]]

  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilder.Result[B, Out]): Out = result match {
    case    ElseBuilder.GE      => new IfGEImpl(???)
    case _: ElseBuilder.Unit[_] => ???
  }

  def ElseIf(cond: => GE): ElseIfBuilder[A] = new ElseIfBuilderImpl(cases, () => cond)
}

final class IfImpl[A](protected val cases: List[IfCase[A]]) extends IfImplLike[A]

final class IfCase[+A](val cond: () => GE, branch: () => A)

final class IfGEImpl(protected val cases: List[IfCase[GE]]) extends IfImplLike[GE] with IfGE with GE.Lazy {
  def rate: MaybeRate = ???

  protected def makeUGens: UGenInLike = ???

  def productElement(n: Int): Any = ???

  def productArity: Int = ???

  def canEqual(that: Any): Boolean = ???
}
