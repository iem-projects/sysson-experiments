package at.iem.sysson.experiments

import de.sciss.synth
import de.sciss.synth.ugen.If
import de.sciss.synth.{GE, NestedUGenGraphBuilder, Server, SynthGraph, ugen}

object ScenarioUnit extends App {
  import ScenarioMod.play

  val CREATE_PDF = false

  If.monolithic = false

  lazy val sg = SynthGraph {
    import Ops.stringToLazyCtlFactory
    import ugen._

    val freq: GE = "freq".kr

    // ---- Unit result ----

    If (freq > 600) Then {
      Out.ar(0, Pan2.ar(SinOsc.ar(freq)))
    }

//    If (freq > 400 & freq <= 600) Then {
//      Out.ar(0, SinOsc.ar(freq))
//    } Else {
//      freq.poll(0, "freq")
//    }
  }

  val ug = NestedUGenGraphBuilder.build(sg)

  if (CREATE_PDF) ScenarioMod.print("top", 0, ug)

  Server.run { s =>
    s.dumpOSC()
    println("Should hear nada.")
    val syn = play(ug, args = List("freq" -> 0))
    import synth.Ops._
    Thread.sleep(2000)
    println("Should hear SinOsc.")
    syn.set("freq" -> 700)
    Thread.sleep(2000)
    s.quit()
    sys.exit()
  }
}