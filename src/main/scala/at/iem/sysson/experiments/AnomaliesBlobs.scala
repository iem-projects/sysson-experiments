/*
 *  AnomaliesBlobs.scala
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

package at.iem.sysson
package experiments

import java.awt.geom.{AffineTransform, Area, GeneralPath, Path2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, FileDialog, Graphics, RenderingHints, Shape}
import java.io.File
import javax.swing.Icon

import at.iem.sysson.util.NetCdfFileUtil
import blobDetection.BlobDetection
import de.sciss.file._
import de.sciss.numbers
import de.sciss.pdflitz.{Generate, SaveAction}
import de.sciss.swingplus.ListView
import ucar.ma2

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.swing.{Component, Dialog, Dimension, Frame, Graphics2D, Rectangle, ScrollPane, Swing}

object AnomaliesBlobs {
  val fIn      : File   = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
  val altRange : Range  = 210 to 360 // 390
  val timeRange: Range  =  20 to (116 + 36) //  8 to 104 // 0 to 179 // 390
  val latRange : Range  =  17 to  17 // 13 to  22
  val lonRange : Range  =   3 to   3
  val varName  : String = "Temperature"
  val lonName  : String = "Longitude"
  val latName  : String = "Latitude"
  val timeName : String = "Time"
  val altName  : String = "Altitude"

  def main(args: Array[String]): Unit = run()

  trait BlobLike {
    def blobLeft    : Int
    def blobTop     : Int
    def blobWidth   : Int
    def blobHeight  : Int
    //      def blobMean    : Double
    //      def blobStdDev  : Double
    //      def blobCenterX : Double
    //      def blobCenterY : Double

    def blobRight   : Int = blobLeft  + blobWidth
    def blobBottom  : Int = blobTop   + blobHeight
    def blobSize    : Int = blobWidth * blobHeight
  }

  case class BlobInput(
                        blobLeft    : Int,
                        blobTop     : Int,
                        blobWidth   : Int,
                        blobHeight  : Int,
                        //      blobMean    : Double,
                        //      blobStdDev  : Double,
                        //      blobCenterX : Double,
                        //      blobCenterY : Double,
                        shape       : Area
                      ) extends BlobLike {

  }

  object BlobSlice {
    //      lazy val numFields: Int = BlobSlice(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).productArity
    lazy val numFields: Int = BlobSlice(0, 0, 0, 0, 0).productArity
  }
  case class BlobSlice(
                        //      boxLeft    : Int,
                        boxTop     : Int,
                        //      boxWidth   : Int,
                        boxHeight  : Int,
                        //      boxMean    : Double,
                        //      boxStdDev  : Double,
                        //      boxCenterX : Double,
                        //      boxCenterY : Double,
                        sliceMean     : Double,
                        sliceStdDev   : Double,
                        sliceCenter   : Double
                      ) {

    def boxBottom: Int = boxTop + boxHeight

    def toArray: Array[Float] =
      Array[Float](boxTop, boxHeight, sliceMean.toFloat, sliceStdDev.toFloat, sliceCenter.toFloat)
  }

  object Blob {
    lazy val numBaseFields: Int = Blob(0, 0, 0, 0, 0, Array.empty).productArity - 1
    lazy val totalNumField: Int = numBaseFields + BlobSlice.numFields
  }
  /** @param id           unique blob identifier, positive. if zero, blob data is invalid
    * @param blobLeft     blob beginning in time frames ("horizontally")
    * @param blobTop      blob beginning within time slice (vertically)
    * @param blobWidth    blob duration in time frames
    * @param blobHeight   blob extent within time slice
    * @param slices       blob form
    */
  final case class Blob(id: Int, blobLeft: Int, blobTop: Int, blobWidth: Int, blobHeight: Int,
                        slices: Array[BlobSlice]) extends BlobLike {

    def overlaps(that: Blob): Boolean =
      this.blobLeft < that.blobRight  && this.blobRight  > that.blobLeft &&
        this.blobTop  < that.blobBottom && this.blobBottom > that.blobTop && {
        val left  = math.max(this.blobLeft , that.blobLeft )
        val right = math.min(this.blobRight, that.blobRight)
        var idx   = left
        var found = false
        while (idx < right) {
          val thisSlice = this.slices(idx - this.blobLeft)
          val thatSlice = that.slices(idx - that.blobLeft)
          found = thisSlice.boxTop < thatSlice.boxBottom && thisSlice.boxBottom > thatSlice.boxTop
          idx += 1
        }
        found
      }

    def toArray(sliceIdx: Int): Array[Float] =
      Array[Float](id, blobLeft, blobTop, blobWidth, blobHeight) ++ slices(sliceIdx).toArray

    def fillSlice(sliceIdx: Int, out: Array[Float], outOff: Int): Unit = {
      val data = toArray(sliceIdx)
      System.arraycopy(data, 0, out, outOff, data.length)
    }
  }

  // XXX TODO --- should verify if we have [time][alt] or [alt][time]
  def mkImageGray(data: Vec[Double], timeSize: Int, altSize: Int, lo: Double, hi: Double, pad: Boolean): BufferedImage = {
    val width   = timeSize + (if (pad) 2 else 0)
    val height  = altSize  + (if (pad) 2 else 0)
    val img     = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    // val g   = img.createGraphics()
    //      val yOff    = height + (if (pad) -2 else -1)
    val yOff    = if (pad) 1 else 0
    val xOff    = if (pad) 1 else 0
    for (x <- 0 until timeSize) {
      for (y <- 0 until altSize) {
        val ta  = data(x * altSize + y)
        import numbers.Implicits._
        val v   = ta.clip(lo, hi).linlin(lo, hi, 0, 255).toInt
        val rgb = 0xFF000000 | (v << 16) | (v << 8) | v
        //          img.setRGB(xOff + x, yOff - y, rgb)
        img.setRGB(xOff + x, yOff + y, rgb)
      }
    }
    img
  }

  def mkFrame(img: BufferedImage, title0: String, blobs: Vec[Blob], shapes: Vec[Shape]): Unit = {
    val width   = img.getWidth
    val height  = img.getHeight
    val c: Component = new Component {
      preferredSize = new Dimension(width * 6, height * 3)

      //        private[this] val strkNorm    = new BasicStroke(1f)
      private[this] val strkThick   = new BasicStroke(2f)
      private[this] val strkThick2  = new BasicStroke(4f)

      override def paint(g: Graphics2D): Unit = {
        val pw = peer.getWidth
        val ph = peer.getHeight
        val sx = pw.toDouble/width
        val sy = ph.toDouble/height
        // g.scale(peer.getWidth.toDouble/width, peer.getHeight.toDouble/height)
        // g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        val atImage = AffineTransform.getTranslateInstance(0, ph)
        atImage.scale(sx, -sy)
        //          g.drawImage(img, 0, 0, pw, ph, peer)
        g.drawImage(img, atImage, peer)
        // g.drawImage(img, 0, 0, peer)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)

        // val atScale = AffineTransform.getScaleInstance(pw, ph)
        // val atScale = AffineTransform.getScaleInstance(sx, sy)

        //          val altMin = altRange.min
        //          val altMax = altRange.max
        //          val timMin = timeRange.min // 0
        //          val timMax = timeRange.max // 179

        //          val ln = new Line2D.Double
        //            blobs.zipWithIndex.foreach { case (b, n) =>
        //              val colr = Color.getHSBColor(n.toFloat / blobs.size, 1f, 1f)
        //              // g.setColor(Color.green)
        //              // val shS = atScale.createTransformedShape(sh)
        //              g.setColor(colr)
        //              g.setStroke(strkThick)
        //              g.draw(shS)
        //              b.slices.zipWithIndex.foreach { case (sl, sli) =>
        //                import numbers.Implicits._
        //                //                val y1 = (sl.boxTop          .linlin(0, altMax - altMin, height - 3, 0) + 1.5) * sy
        //                //                val y2 = (sl.boxBottom       .linlin(0, altMax - altMin, height - 3, 0) + 1.5) * sy
        //                val y1 = (sl.boxTop          .linlin(0, altMax - altMin, 0, height - 3) + 1.5) * sy
        //                val y2 = (sl.boxBottom       .linlin(0, altMax - altMin, 0, height - 3) + 1.5) * sy
        //                val x  = ((sli + b.blobLeft) .linlin(0, timMax - timMin, 0, width - 3 ) + 1.5) * sx
        //                ln.setLine(x, y1, x, y2)
        //                g.draw(ln)
        //              }
        //            }

        val atShapes = AffineTransform.getTranslateInstance(1.5, 1.5 + (ph - 3))
        atShapes.scale(pw - 3, -(ph - 3))

        shapes.zipWithIndex.foreach { case (sh, n) =>
          val colr = Color.getHSBColor(n.toFloat / blobs.size, 1f, 1f)
          // g.setColor(Color.green)
          val shS = atShapes.createTransformedShape(sh)
          //            g.setStroke(strkThick2)
          g.setColor(Color.black)
          val shp2 = strkThick2.createStrokedShape(shS)
          //            g.draw(shS)
          g.fill(shp2)
          //            g.setStroke(strkThick)
          g.setColor(colr)
          val shp3 = strkThick.createStrokedShape(shS)
          //            g.draw(shS)
          g.fill(shp3)
        }

        blobs.zipWithIndex.foreach { case (b, n) =>
          val colr = Color.getHSBColor(n.toFloat / blobs.size, 1f, 1f)
          val p = new Path2D.Double
          //            println(f"blob $n")
          var hasMoved = false
          b.slices.zipWithIndex.foreach { case (sl, i) =>
            import numbers.Implicits._
            val x = (b.blobLeft + i + 1.5).linlin(0, width , 0, 1.0)
            val y = (sl.sliceCenter + 1.0).linlin(0, height, 0, 1.0)
            //              println(f"$x%g, $y%g")
            if (!(x.isNaN || y.isNaN)) {
              if (!hasMoved /* i == 0 */) { p.moveTo(x, y); hasMoved = true } else p.lineTo(x, y)
            }
          }
          val shS = atShapes.createTransformedShape(p)
          //            println(shS.getBounds)
          //            g.setStroke(strkThick2)
          g.setColor(Color.black)
          //            g.draw(shS)
          val shp2 = strkThick2.createStrokedShape(shS)
          g.fill(shp2)
          //            g.setStroke(strkThick)
          g.setColor(colr)
          //            g.draw(shS)
          val shp3 = strkThick.createStrokedShape(shS)
          g.fill(shp3)
        }

        //              // Blobs
        //              if (false /* drawBlobs */) {
        //                g.setColor(Color.red)
        //                g.setStroke(strkNorm)
        //                r.setRect(b.xMin * pw, b.yMin * ph, b.w * pw, b.h * ph)
        //                g.draw(r)
        //              }
        //
        //              n += 1
      }
    }
    val f = new Frame {
      title = title0 // s"range $lo-$hi; nb = ${blobs.size}"
      contents = c
      pack().centerOnScreen()
      open()
    }
    val a = new SaveAction((c: Generate.Source) :: Nil) {
      private val views = (c: Generate.Source) :: Nil

      override def apply(): Unit = {
        val viewsL = views // .toList
        val viewO: Option[Generate.Source] = viewsL match {
          case Nil      => None
          case v0 :: Nil => Some(v0)
          case _ =>
            var w = 0
            var h = 0
            viewsL.foreach { view =>
              val p   = if (usePreferredSize) view.preferredSize else view.size
              val pw  = math.min(64, p.width  >> 3)
              val ph  = math.min(64, p.height >> 3)
              w       = math.max(w, pw)
              h       = math.max(h, ph)
            }
            val list  = new ListView((1 to viewsL.size).map(i => s"#$i"))
            val icons = viewsL.zipWithIndex.map({
              case (view, _ /* idx */) =>
                new Icon {
                  def getIconWidth : Int = w
                  def getIconHeight: Int = h

                  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
                    val g2        = g.asInstanceOf[Graphics2D]
                    val atOrig    = g2.getTransform
                    val clipOrig  = g2.getClip
                    g2.clipRect(x, y, w, h)
                    g2.translate(x, y)
                    g2.scale(0.125, 0.125)
                    prepare(view)
                    view.render(g2)
                    g2.setTransform(atOrig)
                    g2.setClip(clipOrig)
                  }
                }
            })(breakOut)
            val lr = new ListView.LabelRenderer[String] {
              def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: String, index: Int): Unit =
                component.icon = icons(index)
            }
            list.renderer = lr
            list.selectIndices(0)
            list.selection.intervalMode = ListView.IntervalMode.Single
            val scroll  = new ScrollPane(list)
            val res     = Dialog.showConfirmation(message = scroll.peer, title = title, optionType = Dialog.Options.OkCancel,
              messageType = Dialog.Message.Plain)
            val selIdx  = list.selection.leadIndex
            if (res == Dialog.Result.Ok && selIdx >= 0) Some(viewsL(selIdx)) else None
        }
        viewO foreach { view =>
          val fDlg  = new FileDialog(null: java.awt.Frame, title, FileDialog.SAVE)
          fDlg.setVisible(true)
          val file  = fDlg.getFile
          val dir   = fDlg.getDirectory
          if (file == null) return
          val fileExt = if (file.endsWith(".pdf")) file else file + ".pdf"
          prepare(view)
          orsonApply(new File(dir, fileExt), view, usePreferredSize = usePreferredSize, margin = margin, overwrite = true)
        }
      }
    }
    //      a.margin = 32
    a.setupMenu(f)
  }

  def calcBlobs(data: Vec[Double], timeSize: Int, altSize: Int, maxOverlaps: Int,
                openFrame: Boolean = false): Array[Array[Float]] = {
    val lo      = 0.0               // XXX TODO --- make user selectable
    val hi      = 3.5               // XXX TODO --- make user selectable
    val width   = timeSize + 2
    val height  = altSize  + 2
    val img     = mkImageGray(data, timeSize = timeSize, altSize = altSize, lo = lo, hi = hi, pad = true)
    val bd      = new BlobDetection(width, height)
    val pixels  = img.getRGB(0, 0, width, height, null, 0, width)
    bd.setPosDiscrimination(true)
    bd.setIsovalue(200f)            // XXX TODO --- make user selectable?
    bd.computeBlobs(pixels)

    val minWidth  =  3.0 / width    // XXX TODO --- make user selectable
    val minHeight = 10.0 / height   // XXX TODO --- make user selectable
    val blobs     = Vector.tabulate(bd.getBlobNb)(bd.getBlob).filter(b => b.w >= minWidth && b.h >= minHeight)

    val blobShapes: Vec[Shape] = blobs.map { b =>
      val p = new GeneralPath // Path2D.Double
      var m = 0
      while (m < b.getEdgeNb) {
        val eA = b.getEdgeVertexA(m)
        val eB = b.getEdgeVertexB(m)
        if (eA != null && eB != null) {
          if (m == 0) {
            val x = eA.x // * pw
            val y = eA.y // * ph
            p.moveTo(x, y)
          } else {
            val x = eB.x // * pw
            val y = eB.y // * ph
            p.lineTo(x, y)
          }
        }
        m += 1
      }
      p.closePath()
      p
    }

    val blobsAll: Vec[Blob] = blobShapes.map { sh =>
      val scale   = AffineTransform.getScaleInstance(width, height)
      val shS     = scale.createTransformedShape(sh)
      val r       = new Rectangle
      val out     = new Area
      //        var size    = 0
      //        var sum     = 0.0
      //        var sumCnt  = 0
      val br          = shS.getBounds
      val blobLeft    = math.max(0, br.x - 1)
      val blobWidth   = math.min(timeSize - blobLeft, br.width)
      val blobRight   = blobLeft + blobWidth
      val blobTop     = math.max(0, br.y - 1)
      val blobHeight  = math.min(altSize  - blobTop , br.height)
      val blobBottom  = blobTop + blobHeight

      val slices      = new Array[BlobSlice](blobWidth)

      var timeIdx     = blobLeft
      var sliceIdx    = 0
      while (timeIdx < blobRight) {
        r.x       = timeIdx + 1
        r.y       = 0
        r.width   = 1
        r.height  = height
        val a     = new Area(shS)
        a.intersect(new Area(r))
        val b     = a.getBounds2D
        //          assert(b.width == 1, s"b = $b; in = $r; b2 = ${a.getBounds2D}")
        //          size     += b.height
        out.add(new Area(b))
        //          val y0  = math.max(0, b.y - 1)
        //          val y1  = y0 + math.min(altSize, b.height)
        //          for (y <- y0 until y1) {
        //            val ta = data(timeIdx * altSize + y)
        //            sum += ta
        //          }
        val boxTop      = math.max(blobTop   , math.floor(b.getMinY).toInt)
        val boxBottom   = math.min(blobBottom, math.ceil (b.getMaxY).toInt)
        val boxHeight   = boxBottom - boxTop
        var boxVIdx     = boxTop
        var sliceSum    = 0.0
        var sliceCenter = 0.0
        var sliceCnt    = 0
        while (boxVIdx < boxBottom) {
          val ta = data(timeIdx * altSize + boxVIdx)
          if (ta > 1.0) {  // XXX TODO --- user customizable
            sliceSum    += ta
            sliceCenter += ta * boxVIdx
            sliceCnt    += 1
          }
          boxVIdx     += 1
        }
        import numbers.Implicits._
        val sliceMean = sliceSum / sliceCnt // boxHeight
        sliceCenter   = (sliceCenter / sliceSum).clip(boxTop, boxBottom - 1)

        boxVIdx   = boxTop
        var sliceStdDev = 0.0
        while (boxVIdx < boxBottom) {
          val ta  = data(timeIdx * altSize + boxVIdx)
          if (ta > 1.0) {  // XXX TODO --- user customizable
          val d = ta - sliceMean
            sliceStdDev += d * d
          }
          boxVIdx += 1
        }
        if (sliceCnt > 0) sliceStdDev = math.sqrt(sliceStdDev / (sliceCnt - 1))

        val slice = BlobSlice(
          boxTop        = boxTop,
          boxHeight     = boxHeight,
          sliceMean     = sliceMean,
          sliceStdDev   = sliceStdDev,
          sliceCenter   = sliceCenter
        )

        slices(sliceIdx) = slice
        timeIdx  += 1
        sliceIdx += 1
      }
      // bloody floating point ops and rounding can lead to difference here
      //        val ri = out.getBounds
      //        assert(ri == br, s"ri = $ri; br = $br")

      BlobInput(
        blobLeft    = blobLeft,
        blobTop     = blobTop,
        blobWidth   = blobWidth,
        blobHeight  = blobHeight,
        shape       = out
      )

      Blob(
        id          = -1,
        blobLeft    = blobLeft,
        blobTop     = blobTop,
        blobWidth   = blobWidth,
        blobHeight  = blobHeight,
        slices      = slices
      )
    }

    // call with shapes sorted by size in ascending order!
    @tailrec def filterOverlaps(rem: Vec[Blob], out: Vec[Blob], id: Int): Vec[Blob] =
    rem match {
      case head +: tail =>
        val numOverlap = tail.count(_.overlaps(head))
        val idNext  = if (numOverlap > maxOverlaps) id  else id + 1
        val outNext = if (numOverlap > maxOverlaps) out else out :+ head.copy(id = id)
        filterOverlaps(rem = tail, out = outNext, id = idNext)

      case _ => out
    }

    val blobFlt = filterOverlaps(blobsAll.sortBy(_.blobSize), out = Vector.empty, id = 1)
      .sortBy(b => (b.blobLeft, b.blobTop))

    //      val numOverlaps = blobUnions.tails.map {
    //        case head +: tail =>
    //          val hr = head.getBounds2D
    //          tail.count(that => hr.intersects(that.getBounds2D))
    //        case _ => 0
    //      } .max
    //
    //      if (numOverlaps > maxOverlaps) {
    //        sys.error(s"More overlapping blobs ($numOverlaps) than allocated for ($maxOverlaps)")
    //      }

    if (openFrame) Swing.onEDT {
      //        val blobUnions = blobFlt.map(_.shape)
      mkFrame(img, title0 = "blobs", blobs = blobFlt, shapes = blobShapes)
    }

    val blobDimSz = Blob.totalNumField * maxOverlaps
    val res       = Array.ofDim[Float](timeSize, blobDimSz)

    val idIndices = 0 until blobDimSz by Blob.totalNumField

    @tailrec def mkArray(timeIdx: Int, activeBefore: Vec[Blob], rem: Vec[Blob]): Unit =
      if (timeIdx < timeSize) {
        val active1 = activeBefore.filterNot(_.blobRight == timeIdx)
        val (activeAdd, remRem) = rem.partition(_.blobLeft == timeIdx)
        val activeNow = active1 ++ activeAdd
        val out       = res(timeIdx)
        val (activeOld, activeNew) = activeNow.partition(activeBefore.contains)
        if (activeOld.nonEmpty) {
          val outBefore = res(timeIdx - 1)
          activeOld.foreach { blob =>
            val sliceIdx  = timeIdx - blob.blobLeft
            val outOff    = idIndices.find(idx => outBefore(idx) == blob.id).get  // same slot as before
            blob.fillSlice(sliceIdx = sliceIdx, out = out, outOff = outOff)
          }
        }
        if (activeNew.nonEmpty) {
          activeNew.foreach { blob =>
            val sliceIdx  = timeIdx - blob.blobLeft
            val outOff    = idIndices.find(idx => out(idx) == 0).get  // empty slot
            blob.fillSlice(sliceIdx = sliceIdx, out = out, outOff = outOff)
          }
        }
        mkArray(timeIdx + 1, activeBefore = activeNow, rem = remRem)
      } else {
        assert(rem.isEmpty)
      }

    mkArray(0, Vector.empty, blobFlt)

    res
  }

  def run(): Unit = {
    import Implicits._

    val fOut = {
      val d0  = userHome / "Documents" / "temp"
      val d1  = file("/") / "data" / "temp"
      val dir = if (d0.isDirectory) d0 else if (d1.isDirectory) d1 else throw new IllegalStateException()
      dir / "_killme.nc"
    } // "test-blobs3.nc"

    val fNC       = openFile(fIn)
    val v         = fNC.variableMap(varName)

    val maxOverlaps = 4 // 8 // 6 // 2

    def blobTest(): Unit = {
      // val width   = timeRange.size + 2
      // val height  = altRange .size + 2

      /*
        TODO: write as NetCDF file with new matrix containing this feature vector components:

        -  in-blob (zero or one; for zero, we ignore the vector/voice)
        -  alt-hi (high bound altitude)
        -  alt-lo (low bound altitude)
        -  centroid(centroid index altitude)
        -  v-energy (vertically weighted energy)
        -  box-energy (as discussed in the TR}).
        -  box-width
        -  blob-total-height
        -  blob-total-width
      
       */

      // we would need to change altitude for the feature vector, so `inDims` would contain the
      // altitude; we also need to be able to access all time slices at once. I think the trick is
      // to include the time then in `inDims`, but "recreate" it in the `outDimsSpec`. Then the
      // transform function will be called with a "altitude-time" plane, iterating through all
      // longitudes and latitudes.

      val inDims        = Vector(timeName, altName)

      val blobName      = "blobs"
      val blobDimSz     = Blob.totalNumField * maxOverlaps
      // println(s"totalNumField =  ${Blob.totalNumField}")
      val blobDimValues = ma2.Array.factory((0 until blobDimSz).toArray)
      val varTime       = fNC.variableMap(timeName)
      val varTime1      = varTime.in(timeName).select(timeRange)
      val timeDimValues = varTime1.read()
      val timeDimSz     = timeDimValues.size.toInt
      // val varAlt        = fNC.variableMap(altName)
      // val altDimValues  = varAlt.read()
      // val altDimSz      = altDimValues.size.toInt

      val createTime    = NetCdfFileUtil.Create(timeName, units = varTime.units, values = timeDimValues)
      createTime.description = varTime.description
      val outDims = Vector(
        createTime,
//        NetCdfFileUtil.Recreate(timeName),
        NetCdfFileUtil.Keep  (lonName),
        NetCdfFileUtil.Keep  (latName),
        NetCdfFileUtil.Create(blobName, units = None, values = blobDimValues)
      )

      /*
          "a function that will transform the variable's data matrix. It is passed the origin
           in the kept dimensions (origin of the output shape minus the created dimensions)
           and an object
           of dimension `inDims.size` and is required to output an object of
           dimension `outDims.filterNot(_.isCopy).size`. The dimensions are sorted to
           correspond with `inDims`. The function is called repeatedly, iterating
           over all other input dimensions except those in `inDims`."

       */

      val sel = v
        .in(timeName).select(timeRange)
        .in(lonName ).select(lonRange)
        .in(latName ).select(latRange)
        .in(altName ).select(altRange)

      val proc = NetCdfFileUtil.transformSelection(fNC, out = fOut, sel = sel, inDims = inDims, outDimsSpec = outDims) {
        case (origin, arr) =>
          assert(arr.shape == Vector(timeDimSz, altRange.size /* altDimSz */),
            s"Shape seen: ${arr.shape.mkString("[", "][", "]")}; expected: [$timeDimSz][${altRange.size /* altDimSz */}]")
          val data      = arr.double1D
          val openFrame = origin == Vector(3, 17) // || origin == Vector(0, 0)
          val arrOut    = calcBlobs(data, timeSize = timeDimSz, altSize = altRange.size /* altDimSz */ ,
            openFrame = openFrame, maxOverlaps = maxOverlaps)
          // XXX TODO --- here's the problem: `transformSelection` creates
          // the new variable with the same data type as the input variable.
          // This may be float or double in our case. If we write float and the
          // variable is double, NetCDF automatically converts the type. We
          // should make the target type configurable in SysSon.
          val res = ma2.Array.factory(arrOut)
//          assert(res.isFloat)
          res
      }

      import scala.concurrent.ExecutionContext.Implicits.global
      proc.start()
      println("_" * 33)
      proc.monitor(printResult = false)
      Await.result(proc, Duration.Inf)
      println("Done.")
      // sys.exit()
    }

    blobTest()
  }

  def orsonApply(file: File, view: Generate.Source, usePreferredSize: Boolean = false, margin: Int = 0,
            overwrite: Boolean = false): Unit = {
    require(!file.exists() || overwrite, s"The specified file $file already exists.")

    val viewSz = if (usePreferredSize) view.preferredSize else view.size
    val w      = viewSz.width  + (margin << 1)
    val h      = viewSz.height + (margin << 1)
    val doc    = new com.orsonpdf.PDFDocument
    val rect   = new Rectangle(w, h)
    val page   = doc.createPage(rect)
    val g2     = page.getGraphics2D
    view.render(g2)
    doc.writeToFile(file)
  }
}