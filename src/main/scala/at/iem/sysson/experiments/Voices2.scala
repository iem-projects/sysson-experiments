/*
 *  Voices2.scala
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

import de.sciss.numbers
import de.sciss.synth
import de.sciss.synth.trace.TraceSynth.Data
import de.sciss.synth.ugen.{Constant, Flatten}
import de.sciss.synth.{GE, Server, Synth}

import scala.concurrent.{ExecutionContext, Future}
import scala.swing.event.ValueChanged
import scala.swing.{Component, FlowPanel, GridPanel, Label, MainFrame, Slider, Swing}
import scala.util.{Failure, Success, Try}

object Voices2 {
  val numTrajectories = 2
  val egAtk           = 0.1
  val egRls           = 1.5

  def main(args: Array[String]): Unit = boot()

  def boot(): Unit = Server.run { s =>
    val syn = start(s)
    import numbers.Implicits._
    Swing.onEDT {
      val frequencies = Array.fill[Float](numTrajectories)(48.midicps)
      val amplitudes  = Array.fill[Float](numTrajectories)(0f)

      def setControls(): Unit = {
        import de.sciss.synth.Ops._
        syn.set("freq" -> frequencies.toVector, "amp" -> amplitudes.toVector)
      }

      val tr: Seq[Component] = (0 until numTrajectories).map { trIdx =>
        val lbFreq = new Label("00000.0")
        def fmtLb(freq: Float): Unit = lbFreq.text = f"$freq%1.1f"
        val ggSlidFreq = new Slider {
          min   = 48
          max   = 127
          value = 48
          listenTo(this)
          reactions += {
            case ValueChanged(_) =>
              val freq = value.midicps
              fmtLb(freq)
              frequencies(trIdx) = freq
              setControls()
          }
        }
        val ggSlidAmp = new Slider {
          min   = 0
          max   = 63
          value = 0
          listenTo(this)
          reactions += {
            case ValueChanged(_) =>
              val amp = if (value == min) 0f else value.linlin(min, max, -20, 0).dbamp
              amplitudes(trIdx) = amp
              setControls()
          }
        }
        lbFreq.preferredSize = lbFreq.preferredSize
        fmtLb(48.midicps)
        new FlowPanel(new Label(s"#${trIdx + 1}:"), ggSlidFreq, lbFreq, ggSlidAmp)
      }
      new MainFrame {
        title = "Voices"
        contents = new GridPanel(numTrajectories, 1) {
          contents ++= tr
        }
        pack().centerOnScreen()
        open()
      }
    }
  }

  implicit class MyGEOps(private val in: GE) extends AnyVal {
    def +: (head: Constant): GE = Flatten(Seq(head, in))
    def :+ (last: Constant): GE = Flatten(Seq(in, last))
    def ++ (that: GE      ): GE = Flatten(Seq(in, that))
  }

  implicit class TraceFutOps(private val in: Future[List[Data]]) extends AnyVal {
    private def handleRes(in: Try[List[Data]]): Unit = in match {
      case Success(list) => list.foreach(_.print())
      case Failure(ex) =>
        ex.printStackTrace()
    }

    def print()(implicit exec: ExecutionContext): Unit = in.onComplete(handleRes)

    def printAndQuit()(implicit exec: ExecutionContext): Unit = in.onComplete { res =>
      handleRes(res)
      Server.default.quit()
      sys.exit()
    }
  }

  def start(s: Server): Synth = {
    import synth._
    import Ops._
    import ugen._
//    import trace.ugen._
    implicit val exec = s.clientConfig.executionContext

//    import TraceOps._

    val df = SynthDef("voices") {
      val numVoices     = numTrajectories * 2

      val stateInKr     = LocalIn.kr(Seq.fill(numVoices * 3)(0))
      var voiceFreq     = Vector.tabulate(numVoices)(i => stateInKr \ i): GE
      var voiceAmp      = Vector.tabulate(numVoices)(i => stateInKr \ (i + numVoices  )): GE
      var voiceOnOff    = Vector.tabulate(numVoices)(i => stateInKr \ (i + numVoices*2)): GE
      val voiceNos      = 0 until numVoices: GE

//      Trace(voiceFreq , "vc-freq-in")
//      Trace(voiceOnOff, "vc-on  -in")

      val freqIn      = "freq".kr(Vector.fill(numTrajectories)(0))
      val ampIn       = "amp" .kr(Vector.fill(numTrajectories)(0))

//      Trace(freqIn, "in-freq")
//      Trace(ampIn , "in-amp")

      val maxDf       = "max-df".kr(100f)

      var activated   = Vector.fill(numVoices)(0: GE): GE

      // for each frequency, find the best past match
      val noFounds = (0 until numTrajectories).map { tIdx =>
        val fIn         = freqIn \ tIdx
        val aIn         = ampIn  \ tIdx
        val isOn        = aIn > 0

        val freqMatch   = (maxDf - (voiceFreq absdif fIn)).max(0)
        val bothOn      = voiceOnOff & isOn
        val bestIn      = 0 +: (freqMatch * (bothOn & !activated))
        val best        = ArrayMax.kr(bestIn)
        val bestIdx     = best.index - 1

        val bestMask    = voiceNos sig_== bestIdx
        activated      |= bestMask
        val bestMaskN   = !bestMask
        voiceFreq       = voiceFreq * bestMaskN + fIn * bestMask
        voiceAmp        = voiceAmp  * bestMaskN + aIn * bestMask

//        Trace(bestIdx, s"m-match $tIdx")

        bestIdx sig_== -1
      }

      for (tIdx <- 0 until numTrajectories) {
        val fIn             = freqIn \ tIdx
        val aIn             = ampIn  \ tIdx
        val isOn            = aIn > 0
        val voiceAvail      = !(activated | voiceOnOff)

        val notFound        = noFounds(tIdx)
        val startTrajectory = notFound & isOn
        val free            = ArrayMax.kr(0 +: (startTrajectory & voiceAvail))
        val freeIdx         = free.index - 1
        val freeMask        = voiceNos sig_== freeIdx
        activated          |= freeMask
        val freeMaskN       = !freeMask
        voiceFreq           = voiceFreq * freeMaskN + fIn * freeMask
        voiceAmp            = voiceAmp  * freeMaskN + aIn * freeMask

//        Trace(freeIdx, s"m-start $tIdx")
      }

      // ---- voice generation ----
      val voiceEnv      = Env.asr(attack = egAtk, release = egRls)
      val voiceEG       = EnvGen.ar(voiceEnv, gate = activated, levelScale = voiceAmp)

      //      val osc = SinOsc.ar(voiceFreq) * voiceEG / numVoices
      //      Out.ar(0, Mix(Pan2.ar(osc, Seq.tabulate(numVoices)(i => (i % 2) * 2 - 1))))

      // ---- state out ----
      val voiceEGOn = A2K.kr(voiceEG) sig_!= 0
      voiceOnOff    = activated | voiceEGOn

//      Trace(voiceFreq , "vc-freq-out")
//      Trace(voiceOnOff, "vc-on  -out")

      val stateOutKr  = Flatten(voiceFreq ++ voiceAmp ++ voiceOnOff)
      LocalOut.kr(stateOutKr)

      // ---- sound generation ----

      val osc = Mix(Pan2.ar(SinOsc.ar(voiceFreq) * voiceEG, (0 until numVoices).map(_.linlin(0, numVoices - 1, -1, 1))))
      Out.ar(0, Limiter.ar(osc))
    }

    df.play(s)
  }
}
