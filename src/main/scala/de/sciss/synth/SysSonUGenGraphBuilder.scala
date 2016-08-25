package de.sciss.synth

import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.ugen.ControlProxyLike

import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object SysSonUGenGraphBuilder extends UGenGraph.BuilderFactory {
  def build(graph: SynthGraph) = {
    val b = new Impl(graph)
    UGenGraph.use(b)(b.build)
  }

  private final class Impl(graph: SynthGraph) extends SysSonUGenGraphBuilder {
    builder =>

    override def toString = s"UGenGraph.Builder@${hashCode.toHexString}"

    def build: UGenGraph = {
      var g = graph
      var controlProxies = ISet.empty[ControlProxyLike]
      while (g.nonEmpty) {
        // XXX these two lines could be more efficient eventually -- using a 'clearable' SynthGraph
        controlProxies ++= g.controlProxies
        g = SynthGraph(g.sources.foreach(_.force(builder))) // allow for further graphs being created
      }
      build(controlProxies)
    }



//    private[this] var _level = 0
//
//    def level: Int = _level
//    def level_=(value: Int): Unit = if (_level != value) {
//        ...
//      _level = value
//    }

    protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph = ???

    def addUGen(ugen: UGen): Unit = ???

    def prependUGen(ugen: UGen): Unit = ???

    def addControl(values: Vec[Float], name: Option[String]): Int = ???

    def visit[U](ref: AnyRef, init: => U): U = ???
  }
}
trait SysSonUGenGraphBuilder extends UGenGraph.Builder {
  protected def build(controlProxies: Iterable[ControlProxyLike]): UGenGraph

//  var level: Int


}