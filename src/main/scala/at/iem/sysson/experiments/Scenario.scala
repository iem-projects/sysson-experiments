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
      val freq: GE = "freq".kr

      val res0: GE = If (freq > 1000) Then {
        SinOsc.ar(freq)
      } ElseIf (freq > 100) Then {
        Dust.ar(freq)
      } Else {
        WhiteNoise.ar
      }

      Out.ar(0, Pan2.ar(res0 * amp))
    }

    val ug = sg2.expand(SysSonUGenGraphBuilder)

    val sd = SynthDef("test", ug)

//    import at.iem.scalacollider.ScalaColliderDOT
//    val dotC        = ScalaColliderDOT.Config()
//    dotC.input      = sd.graph
//    dotC.graphName  = sd.name
//    val dot         = ScalaColliderDOT(dotC)
//    println(dot)

    import Ops._
    Server.run { s =>
      println("Should hear WhiteNoise.")
      val syn = sd.play(args = List("freq" -> 0))
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
