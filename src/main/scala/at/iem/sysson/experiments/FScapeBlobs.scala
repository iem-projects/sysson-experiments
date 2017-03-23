package at.iem.sysson.experiments

import de.sciss.fscape.Graph
import de.sciss.lucre.synth.InMemory

object FScapeBlobs {
  type S = InMemory



  def mkGraph() = Graph {
    import de.sciss.fscape._
    import graph._
    import at.iem.sysson.fscape.graph._

    val mIn       = Matrix("anom")
    val d1        = Dim(mIn, "time")
    val d2        = Dim(mIn, "altitude")
    val voices    = 4
    val blobSz    = voices * 10
    val specIn    = mIn.spec
    val d3        = Dim.Def("blobs", values = ArithmSeq(length = blobSz))
    val s1        = specIn.moveLast(d1)
    val s2        = s1    .drop    (d2)
    val specOut   = s2    .append  (d3)
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
    val winEl     = BufferDisk(win)
    val blobs     = Blobs2D(in = win, width = width, height = height, thresh = thresh, pad = 1)
    val minWidth  = width * 0.25 // XXX TODO --- has no influence;? XXX TODO --- make user selectable
    val minHeight = 8.0 // XXX TODO --- make user selectable

    val numBlobs    = blobs.numBlobs
    val mOut        = BlobVoices(in = winEl, width = width, height = height,
      numBlobs = numBlobs, bounds = blobs.bounds, numVertices = blobs.numVertices, vertices = blobs.vertices,
      minWidth = minWidth, minHeight = minHeight, voices = voices)
    // val winSzOut  = blobSz * height

    // val frames = MatrixOut("out", specOut, mOut)
    MkMatrix("out", specOut, mOut)

    // frames.poll(Metro(winSzOut).tail, "advance")
    RunningSum(numBlobs).last.poll(0, "total-blobs")
  }
}
