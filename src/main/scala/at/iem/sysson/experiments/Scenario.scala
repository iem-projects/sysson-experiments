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
import de.sciss.osc

import scala.language.implicitConversions

object Scenario {
  implicit def stringToLazyCtlFactory(name: String): LazyControl.Factory = new LazyControl.Factory(name)

  object LazyControl {
    final class Factory(name: String) {
      def ir                       : LazyControl = ir(0f)
      def ir(values: ControlValues): LazyControl = LazyControl(scalar , name, values)
      def kr                       : LazyControl = kr(0f)
      def kr(values: ControlValues): LazyControl = LazyControl(control, name, values)
      def ar                       : LazyControl = ar(0f)
      def ar(values: ControlValues): LazyControl = LazyControl(audio  , name, values)
    }
  }
  final case class LazyControl(rate: Rate, name: String, values: ControlValues) extends GE with Lazy {
    def expand: UGenInLike = UGenGraph.builder.visit(this, init)

    private def init: UGenInLike = rate match {
      case `scalar`   => ControlProxy     (scalar , values.seq, Some(name))
      case `control`  => ControlProxy     (control, values.seq, Some(name))
      case `audio`    => AudioControlProxy(         values.seq, Some(name))
      case _          => sys.error(s"Unsupported LazyControl rate $rate")
    }

    def force(b: UGenGraph.Builder): Unit = ()
  }

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
//      val freq: GE = ExpRand(10, 10000) // XXX TODO --- control currently doesn't work in child branches
      val freq: GE = "freq".kr

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
      var defs  = List.empty[SynthDef]
      var defSz = 0   // used to create unique def names
      var msgs  = List.empty[osc.Message]
      var ctl   = args.reverse  // init
      var buses = List.empty[Bus]

      def loop(child: SysSonUGenGraphBuilder.Result, parent: Group, addAction: AddAction): Node = {
        val name        = s"test-$defSz"
        val sd          = SynthDef(name, child.graph)
        defs          ::= sd
        defSz          += 1
        val syn         = Synth(s)
        val hasChildren = child.children.isEmpty
        val group       = if (!hasChildren) parent else {
          val g   = Group(s)
          msgs  ::= g.newMsg(parent, addAction)
          g
        }
        val node  = if (hasChildren) group else syn
        msgs ::= syn.newMsg(name, target = group, addAction = if (hasChildren) addToHead else addAction)

        child.children.foreach { cc =>
          val ccn = loop(cc, group, addToTail)
          ctl ::= SysSonUGenGraphBuilder.pauseNodeCtlName(cc.id) -> ccn.id
        }

        child.links.foreach { link =>
          val bus = link.rate match {
            case `audio`    => Bus.audio  (s, numChannels = link.numChannels)
            case `control`  => Bus.control(s, numChannels = link.numChannels)
            case other      => throw new IllegalArgumentException(s"Unsupported link rate $other")
          }
          buses ::= bus
          ctl   ::= SysSonUGenGraphBuilder.linkCtlName(link.id) -> bus.index
        }

        node
      }

      val mainNode = loop(res0, parent = s.defaultGroup, addAction = addToHead)
      mainNode.onEnd {
        buses.foreach(_.free())
      }

      msgs ::= mainNode.setMsg(ctl.reverse: _*)
      val b1 = osc.Bundle.now(msgs.reverse: _*)
      val defL :: defI = defs
      val async = defL.recvMsg(b1) :: defI.map(_.recvMsg)
      val b2 = osc.Bundle.now(async.reverse: _*)

      s ! b2
      mainNode
    }

    Server.run { s =>
      s.dumpOSC()
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
