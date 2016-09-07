package at.iem.sysson.experiments

import de.sciss.synth
import de.sciss.synth.GE

// ---- OLD ----

trait IfBuilderOLD {
  def Then [A](branch: => A): IfOLD[A]
}

trait IfOLD[A] {
  def Else [B >: A, Out](branch: => B)(implicit result: ElseBuilderOLD.Result[B, Out]): Out
  def ElseIf (cond: GE): ElseIfBuilderOLD[A]
}

object ElseBuilderOLD {
  object Result extends LowPri {
    implicit def GE: ElseBuilderOLD.GE.type = ElseBuilderOLD.GE
  }
  sealed trait Result[-A, Out]

  object GE           extends Result[synth.GE, synth.GE  ]
  final class Unit[A] extends Result[A       , scala.Unit]

  trait LowPri {
    implicit final def Unit[A]: Unit[A] = instance.asInstanceOf[Unit[A]]
    private final val instance = new Unit[Any]
  }
}

trait ElseIfBuilderOLD[A] {
  def Then [B >: A](branch: => B): IfOLD[B]
}