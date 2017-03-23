package at.iem.sysson.experiments

import java.awt.{Color, Shape}
import java.awt.geom.{AffineTransform, GeneralPath}
import java.awt.image.BufferedImage

import at.iem.sysson.WorkspaceResolver
import at.iem.sysson.experiments.AnomaliesBlobs.Blob
import at.iem.sysson.fscape.GenViewFactory
import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape
import de.sciss.kollflitz.Vec
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.{DataSource, Dimension, Reduce, Matrix => MMatrix}
import de.sciss.lucre.swing.defer
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

object FScapeBlobs {
  type S = InMemory

  def main(args: Array[String]): Unit = {
    implicit val cursor: InMemory = InMemory()
    FScape.init()
    GenViewFactory.install()
    import WorkspaceHandle.Implicits.dummy
    implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
//    val nc      = sysson.openFile(AnomaliesBlobs.fIn)
    val fut     = cursor.step { implicit tx =>
      val (fsc, _fut) = mkSonif()
      implicit val genCtx = GenContext[S]
      val r = Sonification.render[S](fsc)
      r.reactNow { implicit tx => state => if (state.isComplete) {
        r.result.get match {
          case Success(_) =>
            println("Success.")

          case Failure(ex) =>
            println("Failure:")
            ex.printStackTrace()
            sys.exit(1)
        }
      }}

      _fut
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    fut.foreach { case (shapes, img) =>
      println(s"Shapes: ${shapes.size}")
//      sys.exit()
      defer {
        val height  = AnomaliesBlobs.altRange .size
        val width   = AnomaliesBlobs.timeRange.size
//        val img     = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
//        val g       = img.createGraphics()
//        g.setColor(Color.black)
//        g.fillRect(0, 0, width, height)
//        g.dispose()
//        shapes.map(_.getBounds.getSize).foreach(dim => println(s"w = ${dim.width}, h = ${dim.height}"))
        val atScale = AffineTransform.getScaleInstance(1.0 / width, 1.0 / height)
//        atScale.rotate(math.Pi * 0.5, 0.5 * h, 0.5 * w)
        val minWidth  =  3.0 // XXX TODO --- make user selectable
        val minHeight = 10.0 // XXX TODO --- make user selectable
        val shapesSm = shapes.flatMap { sh =>
          val b = sh.getBounds
          if (b.width >= minWidth && b.height >= minHeight) {
            val t = atScale.createTransformedShape(sh)
            Some(t)
          } else None
        }
        val blobs = Vector.tabulate(shapesSm.size) { i =>
          Blob(i, 0, 0, 1, 1, Array.empty)
        }
        AnomaliesBlobs.mkFrame(img, "FScape", blobs, shapesSm)
      }
    }
  }

  implicit class MatrixOps(vr: MMatrix[S]) {
    def in(dimName: String) = new MatrixInOps(vr, dimName)
  }

  class MatrixInOps(vr: MMatrix[S], dimName: String) {
    def select(range: Range)(implicit tx: S#Tx): MMatrix[S] =
      Reduce(vr, Dimension.Selection.Name(dimName), Reduce.Op.Slice(range.head, range.last))
  }

  type Result = (Vec[Shape], BufferedImage)

  def mkSonif()(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): (FScape[S], Future[Result]) = {
    import AnomaliesBlobs._
//    val son           = Sonification[S]
    val (graph, fut)  = mkGraph()
    val fsc           = FScape[S]
    fsc.graph()       = graph
    val loc           = ArtifactLocation.newConst[S](fIn.parent)
    val art           = Artifact(loc, fIn)

//    implicit val resolver: DataSource.Resolver[S] = DataSource.Resolver.seq(nc)

    val ds            = DataSource(art)
    val Some(vr)      = ds.variables.find(_.name == varName)
    val red = vr
      .in(timeName).select(timeRange)
      .in(lonName ).select(lonRange)
      .in(latName ).select(latRange)
      .in(altName ).select(altRange)

//    val src = Sonification.Source(red)
//    val dimMap = src.dims.modifiableOption.get
//    dimMap.put("time"    , timeName)
//    dimMap.put("altitude", altName )
//    val srcMap = son.sources.modifiableOption.get
//    srcMap.put("anom", src)
//    son
    fsc.attr.put("anom", red)

    (fsc, fut)
  }

  def mkGraph(): (Graph, Future[Result]) = {
    var arrNumVertices: Array[Double] = null
    var arrVertices   : Array[Double] = null
    var arrImage      : Array[Double] = null

    val p = Promise[Result]()

    def checkResult(): Unit = {
      if (arrNumVertices != null && arrVertices != null && arrImage != null) {
        val all   = arrVertices.grouped(2).map { arr => (arr(0), arr(1)) }
        val blobs = arrNumVertices.iterator.map { d =>
          val sz  = d.toInt
          val gp  = new GeneralPath
          var i   = 0
          while (i < sz) {
//            val (x, y) = all.next()
            val (y, x) = all.next()
            if (i == 0) gp.moveTo(x, y) else gp.lineTo(x, y)
            i += 1
          }
          gp.closePath()
          gp: Shape
        } .toVector

        val width  = AnomaliesBlobs.timeRange.size
        val height = AnomaliesBlobs.altRange .size
        assert(arrImage.length == width * height)
        val img = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
        for (x <- 0 until width) {
          for (y <- 0 until height) {
            val value     = arrImage(x * height + y)  // rotate at the same time
            val value255  = (math.max(0, math.min(1, value)) * 255).toInt
            val rgb       = (value255 << 16) | (value255 << 8) | value255
            img.setRGB(x, y, rgb)
          }
        }

        p.trySuccess((blobs, img))
      }
    }

    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import graph._

      val mIn       = Matrix("anom")
      import AnomaliesBlobs.{altName, timeName}
      val d1        = Dim(mIn, timeName /* "time" */)
      val d2        = Dim(mIn, altName /* "altitude" */)
//      val voices    = 4
//      val blobSz    = voices * 10
//      val specIn    = mIn.spec
//      val d3        = Dim.Def("blobs", values = ArithmSeq(length = blobSz))
//      val s1        = specIn.moveLast(d1)
//      val s2        = s1    .drop    (d2)
//      val specOut   = s2    .append  (d3)
      val win0      = mIn.valueWindow(d1, d2)  // row-dim, col-dim

      val width     = d2.size
      val height    = d1.size
      // val winSzIn   = width * height

      val taLo      = 0.0
      val taHi      = 3.5
      val win1      = win0.max(taLo).min(taHi) / taHi
      val win       = Gate(win1, !win1.isNaN)

      // width.poll(0, "width")

      val thresh    = 0.26 // 0.14 // 0.21 // 0.26
//      val winEl     = BufferDisk(win)
      val blobs     = Blobs2D(in = win, width = width, height = height, thresh = thresh, pad = 1)
//      val minWidth  = width * 0.25 // XXX TODO --- has no influence;? XXX TODO --- make user selectable
//      val minHeight = 8.0 // XXX TODO --- make user selectable

      val numBlobs    = blobs.numBlobs
//      val mOut        = BlobVoices(in = winEl, width = width, height = height,
//        numBlobs = numBlobs, bounds = blobs.bounds, numVertices = blobs.numVertices, vertices = blobs.vertices,
//        minWidth = minWidth, minHeight = minHeight, voices = voices)
      // val winSzOut  = blobSz * height

      // val frames = MatrixOut("out", specOut, mOut)
      // MkMatrix("out", specOut, mOut)

      // frames.poll(Metro(winSzOut).tail, "advance")
      RunningSum(numBlobs).last.poll(0, "total-blobs")
      width.poll(0, "width")
      height.poll(0, "height")
      Length(win).poll(0, "image-size")

      Collect(blobs.numVertices) { arr => arrNumVertices = arr; checkResult() }
      Collect(blobs.vertices   ) { arr => arrVertices    = arr; checkResult() }
      Collect(win              ) { arr => arrImage       = arr; checkResult() }
    }

    (g, p.future)
  }
}
