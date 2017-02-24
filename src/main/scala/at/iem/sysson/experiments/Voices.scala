/*
 *  Voices.scala
 *  (SysSon-Experiments)
 *
 *  Copyright (c) 2016-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.experiments

import de.sciss.equal.Implicits._
import de.sciss.synth
import de.sciss.synth.trace.TraceSynth.Data
import de.sciss.synth.trace.{BundleBuilder, TraceOps}
import de.sciss.synth.ugen.{Constant, Flatten}
import de.sciss.synth.{GE, Server}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Voices {
  def main(args: Array[String]): Unit = boot()

  def boot(): Unit = Server.run(s => booted(s))

  implicit class MyGEOps(private val in: GE) extends AnyVal {
    def +: (head: Constant): GE = Flatten(Seq(head, in))
    def :+ (last: Constant): GE = Flatten(Seq(in, last))
    def ++ (that: GE): GE = Flatten(Seq(in, that))
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

  def booted(s: Server): Unit = {
    import synth._
    import ugen._
    import Ops._
    import trace.ugen._
    implicit val exec = s.clientConfig.executionContext

    val group = Group.head(s)

    def feedControl(sig: Vec[Vec[Float]], ctl: String, rate: Rate, bundle: BundleBuilder,
                    doneAction: DoneAction = freeSelf): Unit = {
      val numFrames   = sig.size
      val numChannels = sig.head.size

      // println(s"Control $ctl - numChannels $numChannels numFrames $numFrames")

      val df = SynthDef(s"feed_$ctl") {
        val buf   = "buf".ir
        val bus   = "bus".ir
        val play  = PlayBuf(rate, numChannels = numChannels, buf = buf, loop = 0, doneAction = doneAction)
        Out(rate, bus = bus, in = play)
      }
      val buf       = Buffer(s)
      val syn       = Synth(s)
      val setMsg    = buf.setnMsg(sig.flatten)
      val allocMsg  = buf.allocMsg(numFrames = numFrames, numChannels = numChannels, completion = setMsg)
      val bus       = if (rate === audio) Bus.audio(s, numChannels) else Bus.control(s, numChannels)
      val synArgs   = List[ControlSet]("buf" -> buf.id, "bus" -> bus.index)
      val newMsg    = syn.newMsg(df.name, target = group, addAction = addToHead, args = synArgs)
      val mapMsg    = if (rate === audio) group.mapaMsg(ctl -> bus.index) else group.mapMsg(ctl -> bus.index)
      bundle.addAsync(df.recvMsg)
      bundle.addAsync(allocMsg)
      bundle.addSyncToEnd(newMsg)
      bundle.addSyncToEnd(mapMsg)
      syn.onEnd {
        buf.free()
        bus.free()
      }
    }

    import TraceOps._

    val numTraj     = 2 // 4

    val g = traceGraph {
      val numVoices     = numTraj * 2
      val releaseBlocks = 1 // 2 // 4

      val stateInKr     = LocalIn.kr(Seq.fill(numVoices * 2)(0))
      var voiceFreq     = Vector.tabulate(numVoices)(i => stateInKr \ i): GE
      var voiceOnOff    = Vector.tabulate(numVoices)(i => stateInKr \ (i + numVoices  )): GE
      val voiceNos      = 0 until numVoices: GE

      Trace(voiceFreq , "vc-freq-in")
      Trace(voiceOnOff, "vc-on  -in")

      val freqIn      = "freq".kr(Vector.fill(numTraj)(0))
      val ampIn       = "amp" .kr(Vector.fill(numTraj)(0))

      Trace(freqIn, "in-freq")
      Trace(ampIn , "in-amp")

      val maxDf       = "max-df".kr(100f)

      var activated   = Vector.fill(numVoices)(0: GE): GE

      // for each frequency, find the best past match
      val noFounds = (0 until numTraj).map { tIdx =>
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
        voiceFreq       = voiceFreq * !bestMask + fIn * bestMask

        Trace(bestIdx, s"m-match $tIdx")

        bestIdx sig_== -1
      }

      for (tIdx <- 0 until numTraj) {
        val fIn         = freqIn \ tIdx
        val aIn         = ampIn  \ tIdx
        val isOn        = aIn > 0
        val voiceAvail  = !(activated | voiceOnOff)

        val notFound    = noFounds(tIdx)
        val startTraj   = notFound & isOn
        val free        = ArrayMax.kr(0 +: (startTraj & voiceAvail))
        val freeIdx     = free.index - 1
        val freeMask    = voiceNos sig_== freeIdx
        activated      |= freeMask
        voiceFreq       = voiceFreq * !freeMask + fIn * freeMask

        Trace(freeIdx, s"m-start $tIdx")
      }

      // ---- voice generation ----
      val voiceEnv      = Env.asr(attack = ControlDur.ir, release = ControlDur.ir * releaseBlocks)
      val voiceEG       = EnvGen.ar(voiceEnv, gate = activated)

//      val osc = SinOsc.ar(voiceFreq) * voiceEG / numVoices
//      Out.ar(0, Mix(Pan2.ar(osc, Seq.tabulate(numVoices)(i => (i % 2) * 2 - 1))))

      // ---- state out ----
      val voiceEGOn = A2K.kr(voiceEG) sig_!= 0
      voiceOnOff    = activated | voiceEGOn

      Trace(voiceFreq , "vc-freq-out")
      Trace(voiceOnOff, "vc-on  -out")

      val stateOutKr  = Flatten(voiceFreq ++ voiceOnOff)
      LocalOut.kr(stateOutKr)
    }

    val bundle = new BundleBuilder

    val freqCtl = Vec[Vec[Float]](
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(300f, 500f), Vec(300f, 500f),
      Vec(300f, 500f), Vec(300f, 500f),
      Vec(500f, 300f), Vec(700f, 300f),
      Vec(700f, 300f), Vec(500f, 300f)
    )
    val ampCtl = Vec[Vec[Float]](
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(  1f,   1f), Vec(  1f,   1f),
      Vec(  1f,   1f), Vec(  1f,   1f),
      Vec(  1f,   1f), Vec(  1f,   1f)
    )
    feedControl(freqCtl, "freq", control, bundle)
    feedControl(ampCtl , "amp" , control, bundle)

    println("Tracing...")
    val fut = g.traceFor(target = group, addAction = addToTail, fadeTime = -1, numBlocks = 10, bundle = bundle)
    fut.printAndQuit()
  }
}
