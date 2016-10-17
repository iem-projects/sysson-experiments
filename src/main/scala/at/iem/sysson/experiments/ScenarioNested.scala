/*
 *  ScenarioNested
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

package at.iem.sysson.experiments

import at.iem.sysson.experiments.old.Ops._
import de.sciss.synth._
import de.sciss.synth.ugen._

object ScenarioNested extends App {
  val CREATE_PDF = true

//  If.monolithic = false

  lazy val sg = SynthGraph {
    val amp : GE = "amp" .kr(0.2)
    val freq: GE = LFTri.kr(1.0 / 20, iphase = 3).linexp(-1, 1, 10, 1000)

    val res0: GE = IfLag (freq > 100, 0.5) Then {
      val gate0 = ThisBranch()
      val res1: GE = IfLag (freq > 333, 0.5) Then {
        val gate  = ThisBranch()
        // gate.poll(4, "sin")
        val env   = EnvGen.ar(Env.asr(attack = 1.0, release = 0.5, curve = Curve.lin), gate = gate)
        val freq1 = Gate.kr(freq, gate)
        SinOsc.ar(freq1) * 0.2 * env
      } Else {
        val gate  = ThisBranch()
        // gate.poll(4, "saw")
        val env   = EnvGen.ar(Env.asr(attack = 1.0, release = 0.5, curve = Curve.lin), gate = gate)
        val freq1 = Gate.kr(freq, gate)
        Saw.ar(freq1) * 0.2 * env
      }
      // gate.poll(10, "sine-gate")
      //      val env   = Sweep.ar(gate, 1.0/2).min(1)
      val env = EnvGen.ar(Env.asr(attack = 1.0, release = 0.5, curve = Curve.lin), gate = gate0)
      res1 * env

    } Else {
      val gate  = ThisBranch()
      // gate.poll(10, "noise-gate")
      //      val env = Sweep.ar(gate, 1.0/2).min(1)
      val env   = EnvGen.ar(Env.asr(attack = 1.5, release = 0.5, curve = Curve.lin), gate = gate)
      // gate.poll(4, "noise")
      WhiteNoise.ar(0.1) * env
    }

    Out.ar(0, Pan2.ar(res0 * amp))
  }

  lazy val _ = SynthGraph {
    val freq: GE = Line.kr(0, 1000, 20) // LFTri.kr(1.0 / 20, iphase = 3).linexp(-1, 1, 100, 1000)

    val res0: GE = If (freq > 333) Then {
      val res1: GE = If (freq > 666) Then {
        SinOsc.ar(440)
      } Else {
        Saw.ar(440)
      }
      res1

    } Else {
      WhiteNoise.ar
    }

    Out.ar(0, Pan2.ar(res0 * 0.2))
  }

  val ug = NestedUGenGraphBuilder.build(sg)

  if (CREATE_PDF) ScenarioMod.mkPDF(ug)

  Server.run { s =>
    // s.dumpOSC()
    /* val syn = */ ScenarioMod.play(ug, args = List("freq" -> 0))
    println("---- white noise ----")
    s.dumpTree(controls = true)
    Thread.sleep(2000)
    s.dumpTree(controls = true)
    Thread.sleep(22000)
    s.quit()
    sys.exit()
  }
}