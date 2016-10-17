/*
 *  BranchOut.scala
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

package de.sciss.synth
package ugen
package impl

import de.sciss.synth.NestedUGenGraphBuilder.ExpIfTop

import scala.collection.immutable.{IndexedSeq => Vec}
import UGenSource._

final case class BranchOut(top: ExpIfTop, bus: GE, in: GE)
  extends UGenSource.ZeroOut with HasSideEffect with IsIndividual with AudioRated {

  protected def makeUGens: Unit = unwrap(this, bus.expand +: in.expand.outputs)

  private[synth] def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1  = matchRateFrom(_args, 1, audio)
    val numCh   = _args.size - 1
    if (top.numChannels < numCh) top.numChannels = numCh
    UGen.ZeroOut("Out", audio, _args1, isIndividual = true)
  }
}