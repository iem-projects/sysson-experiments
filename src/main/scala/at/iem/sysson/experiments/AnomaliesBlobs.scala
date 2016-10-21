/*
 *  AnomaliesBlobs.scala
 *  (SysSon-Experiments)
 *
 *  Copyright (c) 2016 Institute of Electronic Music and Acoustics, Graz.
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

import java.awt.geom.{AffineTransform, Area, Line2D, Path2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints, Shape}

import at.iem.sysson.util.NetCdfFileUtil
import blobDetection.BlobDetection
import de.sciss.file._
import de.sciss.numbers
import ucar.ma2

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.swing.{Component, Dimension, Frame, Graphics2D, Rectangle, Swing}

object AnomaliesBlobs {
  def main(args: Array[String]): Unit = {
    import Implicits._

    val fIn       = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
    val fOut      = userHome / "Documents" / "temp" / "test-blobs3.nc"
    val altRange  = 210 to 360 // 390
    val latRange  =  13 to  22

    val fNC       = openFile(fIn)
    val varName   = "Temperature"
    val lonName   = "Longitude"
    val latName   = "Latitude"
    val timeName  = "Time"
    val altName   = "Altitude"
    val v         = fNC.variableMap(varName)
//    val timeRange =   8 to 176
//    val altRange  = 210 to 360 // 390
//    val sel = v
//      .in(lonName ).select(  3)
//      .in(latName ).select( 17)
//      .in(timeName).select(timeRange)
//      .in(altName ).select(altRange )

//    val order = v.dimensions.map(_.name).mkString("[", "][", "]")
//    println(order)

    // [Time][Longitude][Latitude][Altitude]

//    assert(sel.reducedRank == 2)
//
//    lazy val data = sel.read().double1D
//
//    def plotSlices(): Unit = {
//      import Plotting.Implicits._
//      data.slice(0            , altRange.size    ).plot(title = "slice 1")
//      data.slice(altRange.size, altRange.size * 2).plot(title = "slice 2")
//    }
//
//    def mkImageBW(thresh: Double): BufferedImage = {
//      val width   = timeRange.size
//      val height  = altRange.size
//      val img     = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
//      // val g   = img.createGraphics()
//      for (x <- timeRange.indices) {
//        for (y <- altRange.indices) {
//          val ta  = data(x * altRange.size + y)
//          val b   = ta > thresh
//          val rgb = if (b) 0xFFFFFFFF else 0xFF000000
//          img.setRGB(x, height - y - 1, rgb)
//        }
//      }
//      img
//    }

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

//    def compareThresh(): Unit = {
//      for (thresh <- 0.5 to 2.0 by 0.5 /* 0.25 */) {
//        val width   = timeRange.size
//        val height  = altRange.size
//        val img     = mkImageBW(thresh)
//
//        new Frame {
//          title = s"thresh $thresh"
//          contents = new Component {
//            preferredSize = new Dimension(width * 4, height * 2)
//
//            override def paint(g: Graphics2D): Unit = {
//              // g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
//              g.drawImage(img, 0, 0, peer.getWidth, peer.getHeight, peer)
//            }
//          }
//          pack().centerOnScreen()
//          open()
//        }
//      }
//    }
    
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

//      def toSlice(
////        id          : Int,
////        blobMean    : Double,
////        blobStdDev  : Double,
////        blobCenterX : Double,
////        blobCenterY : Double,
//        boxLeft     : Int,
//        boxTop      : Int,
//        boxWidth    : Int,
//        boxHeight   : Int,
//        boxMean     : Double,
//        boxStdDev   : Double,
//        boxCenterX  : Double,
//        boxCenterY  : Double,
//        sliceMean   : Double,
//        sliceStdDev : Double,
//        sliceCenter : Double
//     ): BlobSlice = new BlobSlice(
////        id          = id,
////        blobLeft    = blobLeft,
////        blobTop     = blobTop,
////        blobWidth   = blobWidth,
////        blobHeight  = blobHeight,
////        blobMean    = blobMean,
////        blobStdDev  = blobStdDev,
////        blobCenterX = blobCenterX,
////        blobCenterY = blobCenterY,
//        boxLeft     = boxLeft,
//        boxTop      = boxTop,
//        boxWidth    = boxWidth,
//        boxHeight   = boxHeight,
//        boxMean     = boxMean,
//        boxStdDev   = boxStdDev,
//        boxCenterX  = boxCenterX,
//        boxCenterY  = boxCenterY,
//        sliceMean   = sliceMean,
//        sliceStdDev = sliceStdDev,
//        sliceCenter = sliceCenter
//      )
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

    def mkFrame(img: BufferedImage, title0: String, blobs: Vec[Blob]): Unit = {
      val width   = img.getWidth
      val height  = img.getHeight
      new Frame {
        title = title0 // s"range $lo-$hi; nb = ${blobs.size}"
        contents = new Component {
          preferredSize = new Dimension(width * 6, height * 3)

          // private[this] val strkNorm  = new BasicStroke(1f)
          private[this] val strkThick = new BasicStroke(2f)

          override def paint(g: Graphics2D): Unit = {
            val pw = peer.getWidth
            val ph = peer.getHeight
             val sx = pw.toDouble/width
             val sy = ph.toDouble/height
            // g.scale(peer.getWidth.toDouble/width, peer.getHeight.toDouble/height)
            // g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
            g.drawImage(img, 0, 0, pw, ph, peer)
            // g.drawImage(img, 0, 0, peer)
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
            g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)

            // val atScale = AffineTransform.getScaleInstance(pw, ph)
            // val atScale = AffineTransform.getScaleInstance(sx, sy)

            val altMin = altRange.min
            val altMax = altRange.max
            val timMin = 0
            val timMax = 179

            val ln = new Line2D.Double
            blobs.zipWithIndex.foreach { case (b, n) =>
              val colr = Color.getHSBColor(n.toFloat / blobs.size, 1f, 1f)
              // g.setColor(Color.green)
              // val shS = atScale.createTransformedShape(sh)
              g.setColor(colr)
              g.setStroke(strkThick)
              // g.draw(shS)
              b.slices.zipWithIndex.foreach { case (sl, sli) =>
                import numbers.Implicits._
//                val y1 = (sl.boxTop          .linlin(0, altMax - altMin, height - 3, 0) + 1.5) * sy
//                val y2 = (sl.boxBottom       .linlin(0, altMax - altMin, height - 3, 0) + 1.5) * sy
                val y1 = (sl.boxTop          .linlin(0, altMax - altMin, 0, height - 3) + 1.5) * sy
                val y2 = (sl.boxBottom       .linlin(0, altMax - altMin, 0, height - 3) + 1.5) * sy
                val x  = ((sli + b.blobLeft) .linlin(0, timMax - timMin, 0, width - 3 ) + 1.5) * sx
                ln.setLine(x, y1, x, y2)
                g.draw(ln)
              }
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
        pack().centerOnScreen()
        open()
      }
    }

    val maxOverlaps = 4 // 8 // 6 // 2

    def calcBlobs(data: Vec[Double], timeSize: Int, altSize: Int, openFrame: Boolean = false): Array[Array[Float]] = {
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

      val blobShapes  = blobs.map { b =>
        val p = new Path2D.Double
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
          val sliceMean = sliceSum / boxHeight
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
        mkFrame(img, title0 = "blobs", blobs = blobFlt)
      }

//      val blobs = blobFlt.map { in =>
//        var timeIdx   = in.blobLeft
//        val slices    = new Array[BlobSlice](in.blobWidth)
//        var sliceIdx  = 0
//        while (sliceIdx < slices.length) {
//          var sum     = 0.0
//          var altIdx  = in.blobTop
//          val altStop = in.blobBottom
//          while (altIdx < altStop) {
//            val ta = data(timeIdx * altSize + altIdx)
//            sum += ta
//            altIdx += 1
//          }
//          val sliceMean = sum / in.bl
//
//          slices(sliceIdx) = BlobSlice(
//            boxLeft     = ...,
//            boxTop      = ...,
//            boxWidth    = ...,
//            boxHeight   = ...,
//            boxMean     = ...,
//            boxStdDev   = ...,
//            boxCenterX  = ...,
//            boxCenterY  = ...,
//            sliceMean   = ...,
//            sliceStdDev = ...,
//            sliceCenter = ...
//          )
//
//          sliceIdx += 1
//          timeIdx  += 1
//        }
//
//        for (timeIdx <- in.blobLeft until in.blobRight) yield {
//
//        }
//      }

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

//      val blobVec = (0 until timeSize).map { timeIdx =>
//        blobShapes.map { sh =>
//          val scale = AffineTransform.getScaleInstance(width, height)
//          val shS   = scale.createTransformedShape(sh)
//          val r     = new Rectangle2D.Double
//          val out   = new Area
//          for (x <- 0 until timeSize) {
//            r.setRect(x + 1, 0, 1, height)
//            val a = new Area(shS)
//            a.intersect(new Area(r))
//            out.add(new Area(a.getBounds2D))
//          }
//          BlobVector(valid = true, low = ..., high = ..., centroid = ..., vEnergy = ..., boxEnergy = ..., boxWidth = ...,
//            width = ..., height = ...)
//        }
//      }

      res
    }

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
      val timeDimValues = varTime.read()
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
//        .in(lonName ).select(  3)
//        .in(latName ).select(latRange) //  17
        .in(altName ).select(altRange )

      val proc = NetCdfFileUtil.transformSelection(fNC, out = fOut, sel = sel, inDims = inDims, outDimsSpec = outDims) {
        case (origin, arr) =>
          assert(arr.shape == Vector(timeDimSz, altRange.size /* altDimSz */),
            s"Shape seen: ${arr.shape.mkString("[", "][", "]")}; expected: [$timeDimSz][${altRange.size /* altDimSz */}]")
          val data      = arr.double1D
          val openFrame = origin == Vector(3, 17) // || origin == Vector(0, 0)
          val arrOut    = calcBlobs(data, timeSize = timeDimSz, altSize = altRange.size /* altDimSz */ , openFrame = openFrame)
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
}