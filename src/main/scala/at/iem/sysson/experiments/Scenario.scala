package at.iem.sysson.experiments

import de.sciss.synth._
import ugen._

object Scenario {
  def main(args: Array[String]): Unit = {
    lazy val sg1 = SynthGraph {
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
      val amp : GE = "amp".kr
      val freq: GE = "freq".kr

      val res0: GE = If (freq > 1000) Then {
        SinOsc.ar(freq)
      } ElseIf (freq > 100) Then {
        Dust.ar(freq)
      } Else {
        WhiteNoise.ar
      }

      Out.ar(0, res0 * amp)
    }

    val ug = sg2.expand(SysSonUGenGraphBuilder)
    println(ug)

//    val sd = SynthDef("test")(sg.expand)
//
//    import Ops._
//    Server.run { _ =>
//      sd.play(args = ???)
//    }
  }
}
