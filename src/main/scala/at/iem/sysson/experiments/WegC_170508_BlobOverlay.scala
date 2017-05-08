package at.iem.sysson.experiments

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFileSpec, AudioFileType, SampleFormat}

import scala.util.{Failure, Success}

object WegC_170508_BlobOverlay {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val baseDir     = userHome / "sysson"/"bounce"/"WegC_170508-bounce"
    val dirIn       = baseDir / "blob-timbre1"
    val dirOut      = baseDir / "blob-overlay"

    // 0: site, 1: min-alti, 2: max-alti, 3: speed, 4: min-freq, 5: max-freq
    val blobTemp    = "QBO_blobs_timbre1_%s_%s-%skm_%smps_0.25-2.5anom_3off_12.5depth_%s-%sHz.aif"
    // 0: min-alti, 1: max-alti, 2: speed, 3: min-freq, 4: max-freq
    val outTemp     = "QBO_blob-timbre1-overlay_%s-%skm_%smps_%s-%sHz.aif"
    val range1      = (18, 36, 300,  600)
    val range2      = (18, 45, 300,  750)
    val range3      = (18, 45, 300, 1009)
    val speeds      = Seq(6, 12)
    val ranges      = Seq(range1, range2, range3)
    val sites       = Seq("2.5S165W", "2.5N165W", "2.5S75W", "2.5N75W", "2.5S15E", "2.5N15E", "2.5S105E", "2.5N105E")

    val gain        = -6.0.dbamp
    val sr          = 48000.0

    require(dirIn .isDirectory)
    require(dirOut.isDirectory && dirOut.children.isEmpty)

    for {
      speed <- speeds
      range <- ranges
    } {
      val (minAlt, maxAlt, minFreq, maxFreq) = range
      val blobFs = sites.map { site =>
                 dirIn  / blobTemp.format(site, minAlt, maxAlt, speed, minFreq, maxFreq)
      }
      val outF = dirOut / outTemp .format(      minAlt, maxAlt, speed, minFreq, maxFreq)

      val g = Graph {
        import de.sciss.fscape._
        import graph._

        require(!outF.exists())

        val blobIns   = blobFs.map(blobF => AudioFileIn(blobF, numChannels = 2): GE)
        val blobIn    = blobIns.reduce(_ + _)
        val mix       = blobIn * gain
        val specOut   = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = sr)
        val frames    = AudioFileOut(outF, specOut, in = mix)
        frames.poll(mix.abs >= 1.0, s"CLIPPED: ${outF.name}")
      }

      val config = stream.Control.Config()
      config.useAsync   = false
      implicit val ctrl = stream.Control(config)
      import ctrl.config.executionContext
      ctrl.run(g)
      ctrl.status.onComplete {
        case Failure(ex) =>
          println(s"Failed for ${outF.name}")
          ex.printStackTrace()

        case Success(_) =>
          println(s"Wrote ${outF.name}")
      }
    }
  }
}