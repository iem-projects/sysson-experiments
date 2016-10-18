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

import java.awt.{BasicStroke, Color, RenderingHints}
import java.awt.geom.{Path2D, Rectangle2D}
import java.awt.image.BufferedImage

import blobDetection.BlobDetection
import de.sciss.file._
import de.sciss.numbers
import de.sciss.synth.swing.Plotting

import scala.swing.{Component, Dimension, Frame, Graphics2D, MainFrame, Swing}

object AnomaliesBlobs {
  def main(args: Array[String]): Unit = {
    import Implicits._

    val fIn = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
    val fNC = openFile(fIn)
    val v   = fNC.variableMap("Temperature")
    val timeRange =   8 to 176
    val altRange  = 210 to 360 // 390
    val sel = v
      .in("Longitude").select(  3)
      .in("Latitude" ).select( 17)
      .in("Time"     ).select(timeRange)
      .in("Altitude" ).select(altRange )

//    val order = v.dimensions.map(_.name).mkString("[", "][", "]")
//    println(order)

    // [Time][Longitude][Latitude][Altitude]

    assert(sel.reducedRank == 2)

    val data = sel.read().double1D

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

    def mkImageGray(lo: Double, hi: Double, pad: Boolean): BufferedImage = {
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

    def blobTest(): Unit = {
      val width   = timeRange.size + 2
      val height  = altRange .size + 2
      val lo      = 0.0
      val hi      = 3.5
      val img     = mkImageGray(lo, hi, pad = true)
      val bd      = new BlobDetection(width, height)
//      bd.setBlobMaxNumber(12 * 4)
      val pixels  = img.getRGB(0, 0, width, height, null, 0, width)
      // bd.setImage(pixels) // 0x--RRGGBB
      bd.setPosDiscrimination(true)
//      bd.blobWidthMin   = 8
//      bd.blobHeightMin  = 16
      bd.setIsovalue(200f)
      bd.computeBlobs(pixels)

      val minWidth  =  3.0 / width
      val minHeight = 10.0 / height
      val blobs     = Vector.tabulate(bd.getBlobNb)(bd.getBlob).filter(b => b.w >= minWidth && b.h >= minHeight)

      

      new Frame {
        title = s"range $lo-$hi; nb = ${blobs.size}"
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
            var n = 0
            while (n < blobs.size) {
              val b = blobs(n) // bd.getBlob(n)
              // Edges
              if (true /* drawEdges */) {
                p.reset()
                var m = 0
                while (m < b.getEdgeNb) {
                  val eA = b.getEdgeVertexA(m)
                  val eB = b.getEdgeVertexB(m)
                  if (eA != null && eB !=null) {
                    if (m == 0) {
                      val x = eA.x * pw
                      val y = eA.y * ph
                      p.moveTo(x, y)
                    } else {
                      val x = eB.x * pw
                      val y = eB.y * ph
                      p.lineTo(x, y)
                    }
                  }
                  m += 1
                }
                p.closePath()
                val colr = Color.getHSBColor(n.toFloat / blobs.size, 1f, 1f)
                // g.setColor(Color.green)
                g.setColor(colr)
                g.setStroke(strkThick)
                g.draw(p)
              }

              // Blobs
              if (false /* drawBlobs */) {
                g.setColor(Color.red)
                g.setStroke(strkNorm)
                r.setRect(b.xMin * pw, b.yMin * ph, b.w * pw, b.h * ph)
                g.draw(r)
              }

              n += 1
            }
          }
        }
        pack().centerOnScreen()
        open()
      }
    }

    Swing.onEDT {
      blobTest()
    }
  }
}