/*
 *  Ops.scala
 *  (SysSon-Experiments)
 *
 *  Copyright (c) 2016 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.experiments.old

import de.sciss.synth.ugen.{AudioControlProxy, ControlProxy, ControlValues}
import de.sciss.synth.{GE, Lazy, Rate, UGenGraph, UGenInLike, audio, control, scalar}

import scala.language.implicitConversions

object Ops {
  implicit def stringToLazyCtlFactory(name: String): LazyControl.Factory = new LazyControl.Factory(name)

  object LazyControl {
    final class Factory(name: String) {
      def ir                       : LazyControl = ir(0f)
      def ir(values: ControlValues): LazyControl = LazyControl(scalar , name, values)
      def kr                       : LazyControl = kr(0f)
      def kr(values: ControlValues): LazyControl = LazyControl(control, name, values)
      def ar                       : LazyControl = ar(0f)
      def ar(values: ControlValues): LazyControl = LazyControl(audio  , name, values)
    }
  }
  final case class LazyControl(rate: Rate, name: String, values: ControlValues) extends GE with Lazy {
    def expand: UGenInLike = UGenGraph.builder.visit(this, init)

    private def init: UGenInLike = rate match {
      case `scalar`   => ControlProxy     (scalar , values.seq, Some(name))
      case `control`  => ControlProxy     (control, values.seq, Some(name))
      case `audio`    => AudioControlProxy(         values.seq, Some(name))
      case _          => sys.error(s"Unsupported LazyControl rate $rate")
    }

    def force(b: UGenGraph.Builder): Unit = ()
  }
}
