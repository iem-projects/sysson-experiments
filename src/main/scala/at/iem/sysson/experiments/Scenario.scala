/*
 *  Scenario.scala
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

import de.sciss.synth._
import ugen._

object Scenario {
  def main(args: Array[String]): Unit = {
    lazy val _ = SynthGraph {
      val amp : GE = "amp".kr
      val freq: GE = "freq".kr

      // ---- Unit result ----

      If (freq > 600) Then {
        Out.ar(0, SinOsc.ar(freq))
      }

      If (freq > 400 & freq <= 600) Then {
        Out.ar(0, SinOsc.ar(freq))
      } Else {
        freq.poll(0, "freq")
      }

      // ---- GE result ----

      val res0: GE = If (freq > 100) Then {
        SinOsc.ar(freq)
      } Else {
        WhiteNoise.ar
      }

      Out.ar(0, res0 * amp)
    }

    val sg2 = SynthGraph {
      val amp : GE = "amp" .kr(0.2)
//      val freq: GE = "freq".kr
      val freq: GE = ExpRand(10, 10000) // XXX TODO --- control currently doesn't work in child branches

      val res0: GE = If (freq > 1000) Then {
        SinOsc.ar(freq)
      } ElseIf (freq > 100) Then {
        Dust.ar(freq)
      } Else {
        WhiteNoise.ar
      }

      Out.ar(0, Pan2.ar(res0 * amp))
    }

    val ug = SysSonUGenGraphBuilder.build(sg2)

//    val sd = SynthDef("test", ug)

    def print(name: String, level: Int, res: SysSonUGenGraphBuilder.Result): Unit = {
      import at.iem.scalacollider.ScalaColliderDOT
      val dotC        = ScalaColliderDOT.Config()
      dotC.input      = res /* sd */.graph
      dotC.graphName  = /* sd. */ name
      val dot         = ScalaColliderDOT(dotC)
      println(dot)
      res.children.zipWithIndex.foreach { case (child, ci) =>
        print(s"level ${level + 1} idx $ci", level + 1, child)
      }
    }

    print("top", 0, ug)

//    import Ops._
//    Server.run { s =>
//      println("Should hear WhiteNoise.")
//      val syn = sd.play(args = List("freq" -> 0))
//      Thread.sleep(2000)
//      println("Should hear Dust.")
//      syn.set("freq" -> 101)
//      Thread.sleep(2000)
//      println("Should hear SinOsc.")
//      syn.set("freq" -> 1001)
//      Thread.sleep(2000)
//      s.quit()
//      sys.exit()
//    }
  }
}
