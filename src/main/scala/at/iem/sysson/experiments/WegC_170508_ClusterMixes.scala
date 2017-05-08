package at.iem.sysson.experiments

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFileSpec, AudioFileType, SampleFormat}

import scala.util.{Failure, Success}

object WegC_170508_ClusterMixes {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val baseDir     = userHome / "sysson"/"bounce"/"WegC_170508-bounce"
    val mixIn       = baseDir / "mix_in"
    val mixOut      = baseDir / "blob-cluster-mix"

    val singapore   = "2.5N105E"
    val avg         = "avg"
    val sites       = Seq(singapore, avg)
    // 0: site, 1: speed, 2: min-anom, 3: min-freq, 4: max-freq
    val blobTemp    = "QBO_blobs_timbre1_%s_18-36km_%smps_%s-2.5anom_3off_12.5depth_%s-%sHz.aif"
    // 0: site, 1: speed, 2: min-freq, 3: max-freq
    val clusterTemp = "QBO_cluster_%s_18-36km_%smps_1-8anom_%s-%sHz.aif"
    // 0: site, 1: speed, 2: min-freq-b, 3: max-freq-b, 4: min-freq-c, 5: max-freq-c
    val outTemp     = "QBO_blob-cluster-mix_%s_18-36km_%smps_%s-%sHz_%s-%sHz.aif"
    val range1      = (300,  600)
    val range2      = (600, 1200)
    val range3      = (300, 1200)
    val speeds      = Seq(3, 6)
    val rangePairs  = Seq((range1, range2), (range2, range1), (range3, range3))
    val minAnomMap  = Map(singapore -> 0.25, avg -> 0.125)

    val blobGain    = +1.5.dbamp
    val clusterGain = -1.5.dbamp
    val sr          = 48000.0

    require(mixIn .isDirectory)
    require(mixOut.isDirectory && mixOut.children.isEmpty)

    for {
      site  <- sites
      speed <- speeds
      rangePair <- rangePairs
    } {
      val ((minFreqB, maxFreqB), (minFreqC, maxFreqC)) = rangePair
      val minAnom   = minAnomMap(site)

      val blobF     = mixIn  / blobTemp   .format(site, speed, minAnom, minFreqB, maxFreqB)
      val clusterF  = mixIn  / clusterTemp.format(site, speed, minFreqC, maxFreqC)
      val outF      = mixOut / outTemp    .format(site, speed, minFreqB, maxFreqB, minFreqC, maxFreqC)

      val g = Graph {
        import de.sciss.fscape._
        import graph._

        require(!outF.exists())

        val blobIn    = AudioFileIn(blobF   , numChannels = 2)
        val clusterIn = AudioFileIn(clusterF, numChannels = 2)
        val mix       = blobIn * blobGain + clusterIn * clusterGain
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