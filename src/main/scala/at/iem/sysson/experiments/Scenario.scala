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
import de.sciss.file._

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
//        WhiteNoise.ar(Seq.fill(2)(1))
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
//      val dot         = ScalaColliderDOT(dotC)
//      println(dot)
      ScalaColliderDOT.writePDF(dotC, file("dot") / s"${name.replace(' ', '_')}.pdf")
      res.children.zipWithIndex.foreach { case (child, ci) =>
        print(s"child ${ci + 1}", level + 1, child)
      }
    }

    print("top", 0, ug)

    def play(res0: SysSonUGenGraphBuilder.Result, args: List[ControlSet]): Node = {
      val s     = Server.default
      val g     = Group(s)



      def loop(res: SysSonUGenGraphBuilder.Result, subCnt: Int) = {
        val name  = s"test-$subCnt"
        val sd    = SynthDef(name, res.graph)
        sd.recvMsg
        ???
      }

      res0.graph
      res0.links
      res0.children

      ???
    }

    Server.run { s =>
      println("Should hear WhiteNoise.")
      val syn = play(ug, args = List("freq" -> 0))
      import Ops._
      Thread.sleep(2000)
      println("Should hear Dust.")
      syn.set("freq" -> 101)
      Thread.sleep(2000)
      println("Should hear SinOsc.")
      syn.set("freq" -> 1001)
      Thread.sleep(2000)
      s.quit()
      sys.exit()
    }
  }
}
