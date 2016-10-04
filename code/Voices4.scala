implicit class MyGEOps(private val in: GE) extends AnyVal {
  def +: (head: Constant): GE = Flatten(Seq(head, in))
  def :+ (last: Constant): GE = Flatten(Seq(in, last))
  def ++ (that: GE): GE = Flatten(Seq(in, that))
}

play {
  DC.ar(Seq.fill(8)(0))
}

val x = play {
  val numTraj     = 2 // 4
  val numVoices   = numTraj * 2

  val stateIn     = LocalIn.kr(Seq.fill(numVoices * 2)(0))
  var voiceFreq   = Vector.tabulate(numVoices)(i => stateIn \ i): GE
  var voiceOnOff  = Vector.tabulate(numVoices)(i => stateIn \ (i + numVoices)): GE
  val voiceNos    = (0 until numVoices): GE
  
  val freqIn      = "freq".kr(Vector.fill(numTraj)(0))
  val ampIn       = "amp" .kr(Vector.fill(numTraj)(0))
  
  val tick        = Impulse.kr("tick".kr(1))
  val maxDf       = "max-df".kr(100)
  
  val trPoll      = "poll".tr
  val trPoll2     = "poll2".tr
  
  var activated   = Vector.fill(numVoices)(0: GE): GE
  
  for (vIdx <- 0 until numVoices) {
    (voiceFreq  \ vIdx).poll(trPoll & (voiceOnOff \ vIdx), s"fr-in [$vIdx]")
  }
//   for (vIdx <- 0 until numVoices) {
//     (voiceOnOff \ vIdx).poll(trPoll, s"on-in [$vIdx]")
//   }

  // for each frequency, find the best past match
  val noFounds = (0 until numTraj).map { tIdx =>
    val fIn   = freqIn \ tIdx
    val aIn   = ampIn  \ tIdx
    val isOn  = aIn > 0
    
    val freqMatch = (maxDf - (voiceFreq absdif fIn)).max(0)
//     freqMatch.poll(trPoll, s"freq-m[$tIdx]")
    val bothOn    = voiceOnOff & isOn
//     bothOn.poll   (trPoll, s"both  [$tIdx]")
//     val bestIn    = 0 +: (freqMatch * (bothOn & !voiceOnOff))
    val bestIn    = 0 +: (freqMatch * (bothOn & !activated))
    val best      = ArrayMax.ar(bestIn)
    val bestIdx   = best.index - 1
    
//     bestIdx   .poll(trPoll2, s"bestIx[$tIdx]")
//     voiceOnOff.poll(trPoll , s"taken [$tIdx]")
    
    val bestMask  = voiceNos sig_== bestIdx
//     voiceOnOff   |= bestMask
    activated    |= bestMask
    voiceFreq     = voiceFreq * !bestMask + fIn * bestMask

    for (vIdx <- 0 until numVoices)   
      (vIdx: GE).poll(trPoll & (bestMask \ vIdx), s"match [$tIdx]")

    bestIdx sig_== -1
  }

  for (tIdx <- 0 until numTraj) {
    val fIn   = freqIn \ tIdx
    val aIn   = ampIn  \ tIdx
    val isOn  = aIn > 0
    
    val notFound  = noFounds(tIdx) // bestIdx sig_== -1
    val startTraj = notFound & isOn
    val free      = ArrayMax.ar(0 +: (startTraj & !activated))
    val freeIdx   = free.index - 1
    val freeMask  = voiceNos sig_== freeIdx
//     voiceOnOff   |= freeMask
    activated    |= freeMask
    voiceFreq     = voiceFreq * !freeMask + fIn * freeMask

//    activated.poll(trPoll & activated, s"activ [$tIdx]")
    for (vIdx <- 0 until numVoices)   
      (vIdx: GE).poll(trPoll & (freeMask \ vIdx), s"activ [$tIdx]")
  }
  
  voiceOnOff = activated  // release unused voices
//   activated.poll(trPoll, "activated")
  
  for (vIdx <- 0 until numVoices) {
    (voiceFreq \ vIdx).poll(trPoll & (voiceOnOff \ vIdx), s"fr-out[$vIdx]")
  }
//   for (vIdx <- 0 until numVoices) {
//     (voiceOnOff \ vIdx).poll(trPoll, s"on-out[$vIdx]")
//   }
  
  val stateOut = Flatten(voiceFreq ++ voiceOnOff)
  LocalOut.kr(stateOut)
}

x.set("poll" -> 1)
// x.set("freq" -> Vector(123f, 345f, 567f, 789f))
x.set("freq" -> Vector(123f, 345f))
x.set("poll2" -> 1)
x.set("poll" -> 1)
// x.set("amp"  -> Vector(1f, 0f, 0f, 0f))
x.set("amp"  -> Vector(0f, 0f))
x.set("poll" -> 1)
x.set("amp"  -> Vector(1f, 0f))
x.set("amp"  -> Vector(1f, 1f))
x.set("poll" -> 1)
// x.set("v-freq" -> Vector(0f, 345f, 567f, 789f, 123f, 0f, 0f, 0f, 0f))
x.set("v-freq" -> Vector(0f, 345f, 567f, 0f, 0f))
x.set("poll" -> 1)
// x.set("v-amp" -> Vector(0f, 1f, 1f, 1f, 1f, 0f, 0f, 0f, 0f))
x.set("v-amp" -> Vector(0f, 1f, 1f, 0f, 0f))

x.set("poll" -> 1)
x.set("amp"  -> Vector(1f, 1f))
x.set("poll" -> 1)
x.set("freq" -> Vector(523f, 325f))
x.set("poll" -> 1)
x.set("amp"  -> Vector(1f, 0f))

x.set("amp"  -> Vector(1f, 0f), "poll" -> 1)

x.set("freq" -> Vector(523f, 325f))
x.set("amp"  -> Vector(1f, 1f))
x.set("poll" -> 1)
x.set("freq" -> Vector(325f, 523f))
""