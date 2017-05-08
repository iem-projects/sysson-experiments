package at.iem.sysson.experiments

import de.sciss.file._

object ConvertToMp3 {
  def main(args: Array[String]): Unit = {
    val baseDir = userHome / "sysson"/"bounce"
    val baseIn  = baseDir / "WegC_170508-bounce"
    val baseOut = baseDir / "WegC_170508-bounce-mp3"
    convertRecursively(baseIn, baseOut)({ case fIn if fIn.ext == "aif" => fIn.replaceExt("mp3").name }) { (fIn, fOut) =>
      import sys.process._
      val title   = fIn.base
      val artist  = "Hanns Holger Rutz"
      val comment = "All rights reserved. https://github.com/iem-projects/sysson"
      require(!fOut.exists())
      val cmd = Seq("lame", "-h", "-b", "256", "--noreplaygain", "--silent",
        "--tt", title, "--ta", artist, "--ty", "2017", "--tc", comment,
        fIn.path, fOut.path)
      println(cmd.mkString(" "))
      cmd.!!
    }
  }

  /** Recursive conversion of files in a directory.
    * Recursively descends into any non-hidden sub-directories of the `baseIn`,
    * reproducing the relative directory structure within `baseOut`.
    *
    * @param baseIn     input directory to start from
    * @param baseOut    output directory to start from. This directory must exist and be empty
    * @param select     partial function to select input files. Only files are passed into this
    *                   function, no directories. If the caller wishes to process that file,
    *                   it should return the output file name (base name without directory).
    * @param process    the function that will be called with the (existing) input file and
    *                   the (not yet existing) output file. The function should synchronously
    *                   create the output file.
    */
  def convertRecursively(baseIn: File, baseOut: File)(select: PartialFunction[File, String])
                        (process: (File, File) => Unit): Unit = {
    require(baseIn.isDirectory && baseOut.isDirectory)

    val selectF  = select.lift
    val children = baseIn.children.flatMap { fIn =>
       selectF(fIn).map { nameOut =>
         val fOut = baseOut / nameOut
         fIn -> fOut
       }
    }
    children.foreach { case (fIn, fOut) =>
      process(fIn, fOut)
    }

    val sub = baseIn.children(d => d.isDirectory && !d.isHidden)
    sub.foreach { dirSubIn =>
      val dirSubOut = baseOut / dirSubIn.name
      dirSubOut.mkdir()
      convertRecursively(dirSubIn, dirSubOut)(select)(process)
    }
  }
}
