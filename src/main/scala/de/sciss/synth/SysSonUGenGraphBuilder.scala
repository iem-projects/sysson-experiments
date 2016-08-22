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
  }
}
trait SysSonUGenGraphBuilder extends BasicUGenGraphBuilder {

}