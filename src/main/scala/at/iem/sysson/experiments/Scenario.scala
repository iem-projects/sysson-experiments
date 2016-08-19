package at.iem.sysson.experiments

import de.sciss.synth._
import ugen._

object Scenario {
  def main(args: Array[String]): Unit = {
    val sg = SynthGraph {
      val amp: GE = ???
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

//      val res0: GE = If (freq > 100) Then {
//        SinOsc.ar(freq)
//      } Else {
//        WhiteNoise.ar
//      }

//      val res1: GE = If (freq > 1000) Then {
//        SinOsc.ar(freq)
//      } ElseIf (freq > 100) Then {
//        Dust.ar(freq)
//      } Else {
//        WhiteNoise.ar
//      }

//      Out.ar(0, res0)
    }

//    val sd = SynthDef("test")(sg.expand)
//
//    import Ops._
//    Server.run { _ =>
//      sd.play(args = ???)
//    }
  }
}
