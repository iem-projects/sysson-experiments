val x = play {
  val numTraj    = 2 // 4
  val numVoices  = numTraj * 2 + 1  // zero'th is dummy
  
//   val voiceOnOff = Vector.fill(numVoices)(0: GE): GE
//   val voiceFreq  = Vector.fill(numVoices)(0: GE): GE

  val voiceFreq  = "v-freq".ar(Vector.fill(numVoices)(0f))
  val voiceOnOff = "v-amp" .ar(Vector.fill(numVoices)(0f))
  
  val freqIn     = "freq".ar(Vector.fill(numTraj)(0))
  val ampIn      = "amp" .ar(Vector.fill(numTraj)(0))
  
  val tick       = Impulse.ar("tick".kr(1))
  val maxDf      = "max-df".ar(100)
  
  var taken      = Vector.fill(numVoices)(0: GE): GE
  val voiceNos   = (0 until numVoices): GE

    val trPoll = "poll".tr
  
  // for each frequency, find the best past match
  for (tIdx <- 0 until numTraj) {
    val fIn   = freqIn \ tIdx
    val aIn   = ampIn  \ tIdx
    val isOn  = aIn > 0
    
//     voiceFreq.poll(1, "voice-freq")
//     fIn      .poll(1, "f-in      ")
    val freqMatch = (maxDf - (voiceFreq absdif fIn)).max(0)
    freqMatch.poll(trPoll, s"freq-m[$tIdx]")
    val bothOn    = voiceOnOff & isOn
    bothOn.poll   (trPoll, s"both  [$tIdx]")
    val best      = ArrayMax.ar(freqMatch * bothOn * !taken)
    taken.poll    (trPoll, s"taken [$tIdx]")
    
    val idxMask   = voiceNos sig_== best.index
    taken        |= idxMask
  }
  
  for (vIdx <- 0 until numVoices) {
    (taken \ vIdx).poll(trPoll, s"taken$vIdx")
  }
}

x.set("poll" -> 1)
// x.set("freq" -> Vector(123f, 345f, 567f, 789f))
x.set("freq" -> Vector(123f, 345f))
x.set("poll" -> 1)
// x.set("amp"  -> Vector(1f, 0f, 0f, 0f))
x.set("amp"  -> Vector(1f, 0f))
x.set("poll" -> 1)
// x.set("v-freq" -> Vector(0f, 345f, 567f, 789f, 123f, 0f, 0f, 0f, 0f))
x.set("v-freq" -> Vector(0f, 345f, 567f, 0f, 0f))
x.set("poll" -> 1)
// x.set("v-amp" -> Vector(0f, 1f, 1f, 1f, 1f, 0f, 0f, 0f, 0f))
x.set("v-amp" -> Vector(0f, 1f, 1f, 0f, 0f))

x.set("poll" -> 1)
x.set("amp"  -> Vector(1f, 1f))
x.set("poll" -> 1)  // OK, voice 1 taken
x.set("freq" -> Vector(523f, 325f))
x.set("poll" -> 1)  // OK, voices 1 and 2 taken

/*
// all identical (e.g. all zero) -> reported index is zero
play {
  ArrayMax.ar(Vector.fill(4)(DC.ar(1))).poll(1, "max")
  ()
}
*/
