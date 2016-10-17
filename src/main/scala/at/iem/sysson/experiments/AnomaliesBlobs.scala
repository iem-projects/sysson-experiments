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

import java.awt.RenderingHints
import java.awt.image.BufferedImage

import de.sciss.file._
import de.sciss.synth.swing.Plotting

import scala.swing.{Component, Dimension, Frame, Graphics2D, MainFrame, Swing}

object AnomaliesBlobs {
  def main(args: Array[String]): Unit = {
    import Implicits._

    val fIn = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
    val fNC = openFile(fIn)
    val v   = fNC.variableMap("Temperature")
    val timeRange =   8 to 176
    val altRange  = 210 to 390
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

    def compareThresh(): Unit = {
      for (thresh <- 0.5 to 2.0 by 0.5 /* 0.25 */) {
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
        // g.dispose()

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

    Swing.onEDT {
      compareThresh()
    }
  }
}