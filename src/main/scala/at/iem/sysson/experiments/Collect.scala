package at.iem.sysson.experiments

import akka.stream.{Attributes, Inlet, Outlet, SinkShape}
import de.sciss.fscape.stream.impl.{NodeImpl, Sink1Impl, StageImpl}
import de.sciss.fscape.stream.{BufD, StreamIn}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenSource, stream}

import scala.collection.immutable.{IndexedSeq => Vec}

object Collect {
  private type Done = Array[Double] => Unit

  private def mkStream(in: Outlet[BufD], done: Done)(implicit b: stream.Builder): Unit = {
    val stage0  = new Stage(done)
    val stage   = b.add(stage0)
    b.connect(in, stage.in)
  }

  private final val name = "Collect"

  private type Shape = SinkShape[BufD]

  private final class Stage(done: Done)(implicit ctrl: stream.Control) extends StageImpl[Shape](name) {
    val shape = SinkShape(
      in = Inlet[BufD](s"$name.in")
    )

    def createLogic(attr: Attributes) = new Logic(done = done, shape = shape)
  }

  private final class Logic(done: Done, shape: Shape)(implicit ctrl: stream.Control)
    extends NodeImpl(name, shape)
      with Sink1Impl[BufD] {

    private[this] val arrB = Array.newBuilder[Double]

    private def shouldComplete: Boolean = isClosed(shape.in) && !isAvailable(shape.in)

    def process(): Unit = {
      if (canRead) {
        val stop0   = readIns()
        val b0      = bufIn0.buf
        val _arrB   = arrB
        var inOffI  = 0
        while (inOffI < stop0) {
          _arrB  += b0(inOffI)
          inOffI += 1
        }
      }

      if (shouldComplete) {
        done(arrB.result())
        completeStage()
      }
    }
  }
}
final case class Collect(in: GE)(done: Array[Double] => Unit) extends UGenSource.ZeroOut {
  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): Unit =
    UGen.ZeroOut(this, args, isIndividual = true)

  protected def makeUGens(implicit b: UGenGraph.Builder): Unit =
    UGenSource.unwrap(this, Vector(UGenSource.expand(in)))

  def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): Unit = {
    val Vec(in) = args
    Collect.mkStream(in.toDouble, done)
  }
}