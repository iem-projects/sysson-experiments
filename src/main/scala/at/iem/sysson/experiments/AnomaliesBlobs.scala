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

import java.awt.geom.{AffineTransform, Area, Path2D, Rectangle2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints, Shape}

import at.iem.sysson.util.NetCdfFileUtil
import blobDetection.BlobDetection
import de.sciss.file._
import de.sciss.numbers
import de.sciss.synth.swing.Plotting
import ucar.ma2

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.swing.{Component, Dimension, Frame, Graphics2D, Swing}

object AnomaliesBlobs {
  def main(args: Array[String]): Unit = {
    import Implicits._

    val fIn       = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
    val fNC       = openFile(fIn)
    val varName   = "Temperature"
    val lonName   = "Longitude"
    val latName   = "Latitude"
    val timeName  = "Time"
    val altName   = "Altitude"
    val v         = fNC.variableMap(varName)
    val timeRange =   8 to 176
    val altRange  = 210 to 360 // 390
    val sel = v
      .in(lonName ).select(  3)
      .in(latName ).select( 17)
      .in(timeName).select(timeRange)
      .in(altName ).select(altRange )

//    val order = v.dimensions.map(_.name).mkString("[", "][", "]")
//    println(order)

    // [Time][Longitude][Latitude][Altitude]

    assert(sel.reducedRank == 2)

    lazy val data = sel.read().double1D

    def plotSlices(): Unit = {
      import Plotting.Implicits._
      data.slice(0            , altRange.size    ).plot(title = "slice 1")
      data.slice(altRange.size, altRange.size * 2).plot(title = "slice 2")
    }

    def mkImageBW(thresh: Double): BufferedImage = {
      val width   = timeRange.size
      val height  = altRange.size
      val img     = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      // val g   = img.createGraphics()
      for (x <- timeRange.indices) {
        for (y <- altRange.indices) {
          val ta  = data(x * altRange.size + y)
          val b   = ta > thresh
          val rgb = if (b) 0xFFFFFFFF else 0xFF000000
          img.setRGB(x, height - y - 1, rgb)
        }
      }
      img
    }

    // XXX TODO --- should verify if we have [time][alt] or [alt][time]
    def mkImageGray(data: Vec[Double], lo: Double, hi: Double, pad: Boolean): BufferedImage = {
      val width   = timeRange.size + (if (pad) 2 else 0)
      val height  = altRange.size  + (if (pad) 2 else 0)
      val img     = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
      // val g   = img.createGraphics()
      val yOff    = height + (if (pad) -2 else -1)
      val xOff    = if (pad) 1 else 0
      for (x <- timeRange.indices) {
        for (y <- altRange.indices) {
          val ta  = data(x * altRange.size + y)
          import numbers.Implicits._
          val v   = ta.clip(lo, hi).linlin(lo, hi, 0, 255).toInt
          val rgb = 0xFF000000 | (v << 16) | (v << 8) | v
          img.setRGB(xOff + x, yOff - y, rgb)
        }
      }
      img
    }

    def compareThresh(): Unit = {
      for (thresh <- 0.5 to 2.0 by 0.5 /* 0.25 */) {
        val width   = timeRange.size
        val height  = altRange.size
        val img     = mkImageBW(thresh)

        new Frame {
          title = s"thresh $thresh"
          contents = new Component {
            preferredSize = new Dimension(width * 4, height * 2)

            override def paint(g: Graphics2D): Unit = {
              // g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              g.drawImage(img, 0, 0, peer.getWidth, peer.getHeight, peer)
            }
          }
          pack().centerOnScreen()
          open()
        }
      }
    }

    case class BlobVector(
      valid      : Boolean,
      low        : Double,
      high       : Double,
      centroid   : Double,
      vEnergy    : Double,
      boxEnergy  : Double,
      boxWidth   : Int,
      width      : Int,
      height     : Double
    )

    def blobTest(): Unit = {
      // val width   = timeRange.size + 2
      // val height  = altRange .size + 2

      val maxOverlaps = 2

      def calcBlobs(data: Vec[Double], timeSize: Int, altSize: Int) = {
        val lo      = 0.0
        val hi      = 3.5
        val width   = timeSize + 2
        val height  = altSize  + 2
        val img     = mkImageGray(data, lo, hi, pad = true)
        val bd      = new BlobDetection(width, height)
        val pixels  = img.getRGB(0, 0, width, height, null, 0, width)
        bd.setPosDiscrimination(true)
        bd.setIsovalue(200f)
        bd.computeBlobs(pixels)

        val minWidth  =  3.0 / width
        val minHeight = 10.0 / height
        val blobs     = Vector.tabulate(bd.getBlobNb)(bd.getBlob).filter(b => b.w >= minWidth && b.h >= minHeight)

        val blobShapes  = blobs.map { b =>
          val p = new Path2D.Double
          var m = 0
          while (m < b.getEdgeNb) {
            val eA = b.getEdgeVertexA(m)
            val eB = b.getEdgeVertexB(m)
            if (eA != null && eB !=null) {
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

        val blobUnions = blobShapes.map { sh =>
          val scale = AffineTransform.getScaleInstance(width, height)
          val shS   = scale.createTransformedShape(sh)
          val r     = new Rectangle2D.Double
          val out   = new Area
          for (x <- timeRange.indices) {
            r.setRect(x + 1, 0, 1, height)
            val a = new Area(shS)
            a.intersect(new Area(r))
            out.add(new Area(a.getBounds2D))
          }
          out: Shape
        }

        val numOverlaps = blobUnions.tails.map {
          case head +: tail =>
            val hr = head.getBounds2D
            tail.count(that => hr.intersects(that.getBounds2D))
          case _ => 0
        } .max

        if (numOverlaps > maxOverlaps) {
          sys.error(s"More overlapping blobs ($numOverlaps) than allocated for ($maxOverlaps)")
        }

        ???
      }

      // println(s"numOverlaps = $numOverlaps")

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

      val fOut = userHome / "Documents" / "temp" / "test.nc"

      // we would need to change altitude for the feature vector, so `inDims` would contain the
      // altitude; we also need to be able to access all time slices at once. I think the trick is
      // to include the time then in `inDims`, but "recreate" it in the `outDimsSpec`. Then the
      // transform function will be called with a "altitude-time" plane, iterating through all
      // longitudes and latitudes.

      val inDims        = Vector(timeName, altName)

      val blobName      = "blobs"
      val blobVecSize   = BlobVector(valid = false, 0, 0, 0, 0, 0, 0, 0, 0).productArity
      val blobDimValues = ma2.Array.factory((0 until (blobVecSize * maxOverlaps)).toArray)
      val blobDimSz     = blobDimValues.size.toInt
      val varTime       = fNC.variableMap(timeName)
      val timeDimValues = varTime.read()
      val timeDimSz     = timeDimValues.size.toInt
      val varAlt        = fNC.variableMap(altName)
      val altDimValues  = varAlt.read()
      val altDimSz      = altDimValues.size.toInt
      val outDims = Vector(
        NetCdfFileUtil.Create(timeName, units = varTime.units, values = timeDimValues),
        NetCdfFileUtil.Keep  (lonName),
        NetCdfFileUtil.Keep  (latName),
        NetCdfFileUtil.Create(blobName, units = None         , values = blobDimValues)
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
      val proc = NetCdfFileUtil.transform(fNC, out = fOut, varName = varName, inDims = inDims, outDimsSpec = outDims) {
        case (origin, arr) =>
          assert(arr.shape == Vector(timeDimSz, altDimSz),
            s"Shape seen: ${arr.shape.mkString("[", "][", "]")}; expected: [$timeDimSz][$altDimSz]")
          ma2.Array.factory(ma2.DataType.FLOAT, Array[Int](blobDimSz, timeDimSz))
      }

      import scala.concurrent.ExecutionContext.Implicits.global
      proc.start()
      proc.monitor(printResult = false)
      Await.result(proc, Duration.Inf)
      println("Done.")
      sys.exit()

      def mkFrame(img: BufferedImage, title0: String, blobUnions: Vec[Shape]): Unit = {
        val width   = img.getWidth
        val height  = img.getHeight
        new Frame {
          title = title0 // s"range $lo-$hi; nb = ${blobs.size}"
          contents = new Component {
            preferredSize = new Dimension(width * 6, height * 3)

            private[this] val strkNorm  = new BasicStroke(1f)
            private[this] val strkThick = new BasicStroke(2f)
            private[this] val p         = new Path2D.Double
            private[this] val r         = new Rectangle2D.Double

            override def paint(g: Graphics2D): Unit = {
              val pw = peer.getWidth
              val ph = peer.getHeight
              // val sx = pw.toDouble/width
              // val sy = ph.toDouble/height
              // g.scale(peer.getWidth.toDouble/width, peer.getHeight.toDouble/height)
              // g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              g.drawImage(img, 0, 0, pw, ph, peer)
              // g.drawImage(img, 0, 0, peer)
              g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
              g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)

              // val atScale = AffineTransform.getScaleInstance(pw, ph)
              val atScale = AffineTransform.getScaleInstance(pw.toDouble / width, ph.toDouble / height)

              blobUnions /* blobShapes */.zipWithIndex.foreach { case (sh, n) =>
                val colr = Color.getHSBColor(n.toFloat / blobUnions.size, 1f, 1f)
                // g.setColor(Color.green)
                val shS = atScale.createTransformedShape(sh)
                g.setColor(colr)
                g.setStroke(strkThick)
                g.draw(shS)
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
    }

    blobTest()
  }
}