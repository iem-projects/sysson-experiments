/*
 *  Voices.scala
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
      val numChannels = sig.size
      val numFrames   = sig.head.size
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
      val numVoices   = numTraj * 2

      val stateIn     = LocalIn.kr(Seq.fill(numVoices * 2)(0))
      var voiceFreq   = Vector.tabulate(numVoices)(i => stateIn \ i): GE
      var voiceOnOff  = Vector.tabulate(numVoices)(i => stateIn \ (i + numVoices)): GE
      val voiceNos    = 0 until numVoices: GE

      Trace(voiceFreq , "voice-freq-in")
      Trace(voiceOnOff, "voice-on  -in")

      val freqIn      = "freq".kr(Vector.fill(numTraj)(0))
      val ampIn       = "amp" .kr(Vector.fill(numTraj)(0))

      Trace(freqIn, "freq-in")
      Trace(ampIn , "amp -in")

      // val tick        = Impulse.kr("tick".kr(1f))
      val maxDf       = "max-df".kr(100f)

      // val trPoll      = "poll".tr
      // val trPoll2     = "poll2".tr

      var activated   = Vector.fill(numVoices)(0: GE): GE

      // for each frequency, find the best past match
      val noFounds = (0 until numTraj).map { tIdx =>
        val fIn   = freqIn \ tIdx
        val aIn   = ampIn  \ tIdx
        val isOn  = aIn > 0

        val freqMatch = (maxDf - (voiceFreq absdif fIn)).max(0)
        val bothOn    = voiceOnOff & isOn
        val bestIn    = 0 +: (freqMatch * (bothOn & !activated))
        val best      = ArrayMax.ar(bestIn)
        val bestIdx   = best.index - 1

        val bestMask  = voiceNos sig_== bestIdx
        activated    |= bestMask
        voiceFreq     = voiceFreq * !bestMask + fIn * bestMask

        Trace(bestIdx, s"f-match $tIdx")

        bestIdx sig_== -1
      }

      for (tIdx <- 0 until numTraj) {
        val fIn   = freqIn \ tIdx
        val aIn   = ampIn  \ tIdx
        val isOn  = aIn > 0

        val notFound  = noFounds(tIdx) // bestIdx sig_== -1
        val startTraj = notFound & isOn
        val free      = ArrayMax.ar(0 +: (startTraj & !activated))
        val freeIdx   = free.index - 1
        val freeMask  = voiceNos sig_== freeIdx
        activated    |= freeMask
        voiceFreq     = voiceFreq * !freeMask + fIn * freeMask

        Trace(freeIdx, s"f-free  $tIdx")
      }

      voiceOnOff = activated  // release unused voices

      Trace(voiceFreq , "voice-freq-out")
      Trace(voiceOnOff, "voice-on  -out")

      val stateOut = Flatten(voiceFreq ++ voiceOnOff)
      LocalOut.kr(stateOut)
      ()
    }

    val bundle = new BundleBuilder

    val freqCtl = Vec[Vec[Float]](
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(300f, 500f), Vec(300f, 500f),
      Vec(300f, 500f), Vec(300f, 500f),
      Vec(500f, 300f), Vec(500f, 300f)
    )
    val ampCtl = Vec[Vec[Float]](
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(  0f,   0f), Vec(  0f,   0f),
      Vec(  1f,   1f), Vec(  1f,   1f),
      Vec(  1f,   1f), Vec(  1f,   1f)
    )
    feedControl(freqCtl, "freq", control, bundle)
    feedControl(ampCtl , "amp" , control, bundle)

    println("Tracing...")
    val fut = g.traceFor(target = group, addAction = addToTail, fadeTime = -1, numBlocks = 8, bundle = bundle)
    fut.printAndQuit()
  }
}
