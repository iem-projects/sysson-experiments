/*
 *  ScenarioThisBranch
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

package at.iem.sysson.experiments

import at.iem.sysson.experiments.Ops._
import de.sciss.synth._
import de.sciss.synth.ugen._

object ScenarioThisBranch extends App {
  val CREATE_PDF = false

  If.monolithic = false

  val sg = SynthGraph {
    val amp : GE = "amp" .kr(0.2)
    val freq: GE = "freq".kr

    val res0: GE = If (freq > 1000) Then {
      val env = Sweep.ar(ThisBranch(), 1.0/2).min(1)
      SinOsc.ar(freq) * 0.2 * env
    } ElseIf (freq > 100) Then {
      freq.poll(ThisBranch(), "branch 2")
      val env = Sweep.ar(ThisBranch(), 1.0/1).min(1)
      Dust.ar(freq) * env
    } Else {
      val env = Sweep.ar(ThisBranch(), 1.0/2).min(1)
      WhiteNoise.ar(0.1) * env
    }

    Out.ar(0, Pan2.ar(res0 * amp))
  }

  val ug = NestedUGenGraphBuilder.build(sg)

  if (CREATE_PDF) ScenarioMod.mkPDF(ug)

  Server.run { s =>
    s.dumpOSC()
    println("Should hear WhiteNoise.")
    val syn = ScenarioMod.play(ug, args = List("freq" -> 0))
    Thread.sleep(2000)
    println("Should hear Dust.")
    import de.sciss.synth.Ops._
    syn.set("freq" -> 123)
    Thread.sleep(2000)
    println("Should hear SinOsc.")
    syn.set("freq" -> 1234)
    Thread.sleep(2000)
    println("Should hear WhiteNoise.")
    syn.set("freq" -> 1)
    Thread.sleep(2000)
    println("Should hear Dust.")
    syn.set("freq" -> 333)
    Thread.sleep(2000)
    println("Should hear SinOsc.")
    syn.set("freq" -> 1001)
    Thread.sleep(2000)
    s.quit()
    sys.exit()
  }
}
