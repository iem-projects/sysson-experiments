package at.iem.sysson.experiments

import at.iem.scalacollider.ScalaColliderDOT
import at.iem.sysson.experiments.old.Ops._
import de.sciss.file._
import de.sciss.synth.ugen.{Dust, If, Out, Pan2, SinOsc, WhiteNoise}
import de.sciss.synth.{GE, SynthDef}

object ScenarioMono extends App {
  val CREATE_PDF = false

//  If.monolithic = true

  val sd = SynthDef("monolith") {
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

  if (CREATE_PDF) {
    val dotC        = ScalaColliderDOT.Config()
    dotC.input      = sd.graph
    dotC.graphName  = sd.name
    dotC.rateColors = true
    ScalaColliderDOT.writePDF(dotC, file("dot") / "mono.pdf")
  }
}
