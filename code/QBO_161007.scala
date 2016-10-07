// version: 07-Oct-2016

implicit class MyGEOps(private val in: GE) /* extends AnyVal */ {
  def +: (head: Constant): GE = Flatten(Seq(head, in))
  def :+ (last: Constant): GE = Flatten(Seq(in, last))
  def ++ (that: GE      ): GE = Flatten(Seq(in, that))
  def \  (r: Range      ): GE = r.map(i => in \ i): GE
}

// ---- matrix ----

val vr    = Var("anom")
val dTime = Dim(vr, "time")
val dAlt  = Dim(vr, "altitude")

val speed = UserValue("speed", 1).kr
val tp    = dTime.play(speed)
val vp    = vr.play(tp, interp = 1 /* 2 */)

// ---- configuration ----

// maximum number of trajectories followed
val numTraj       = 4

// maximum number of concurrently playing voices
val numVoices     = numTraj * 2

// maximum jump in altitude (normalized - full range equals one) before trajectory is interrupted
val maxDf         = 0.2

// trajectory amplitude envelope attack duration
val egAtk         = 0.1

// trajectory amplitude envelope release duration
val egRls         = 1.5

// trajectory frequency and amplitude smear time
val lagTime       = 1.0

// threshold above which temperatures are considered anormal
val magThresh     = 1.5

// maximum magnitude considered for anormal values
val magMax        = 8.0

// sonification minimum oscillator frequency
val minFreq       = 200

// sonification maximum oscillator frequency
val maxFreq       = 4000

// ---- state ----

val stateInKr     = LocalIn.kr(Seq.fill(numVoices * 3 * 2)(0))

val voiceNos      = 0 until numVoices: GE
val numAlt        = dAlt.size
val maskWidth     = numAlt / 2 // 4
val vpChanIdx     = ChannelIndices(vp)

def mkSide(isUp: Boolean): (GE, GE) = {
  // ---- state ----
  val stateOff      = if (isUp) 0 else 3
  var voiceFreq     = stateInKr \ ((numVoices * (stateOff+0)) until (numVoices * (stateOff+1)))
  var voiceAmp      = stateInKr \ ((numVoices * (stateOff+1)) until (numVoices * (stateOff+2)))
  var voiceOnOff    = stateInKr \ ((numVoices * (stateOff+2)) until (numVoices * (stateOff+3)))
  
  // ---- trace trajectories ----
  
  def extract(in: GE, res: Seq[(GE, GE)], trjIdx: Int): Seq[(GE, GE)] = 
    if (trjIdx == numTraj) res else {
      val (bestIdx, bestVal) = 
        if (isUp) {
          val best = ArrayMax.kr(in)
          best.index -> best.value
        } else {
          val best = ArrayMin.kr(in)
          best.index -> best.value
        }

      val freq0     = bestIdx / (numAlt - 1) // .linexp(0, numAlt - 1, 200, 4000)
      val amp0      = if (isUp)
        bestVal.clip(magThresh, magMax).linlin(magThresh, magMax, 0, 1)
      else
        bestVal.clip(-magMax, -magThresh).linlin(-magMax, -magThresh, 1, 0)
      
      val mask      = in * vpChanIdx.absdif(bestIdx) > maskWidth
      extract(in = mask, res = res :+ (freq0 -> amp0), trjIdx = trjIdx + 1)
    }
  
  var activated   = Vector.fill(numVoices)(0: GE): GE
  
  val (freqInSq, ampInSq) = extract(in = A2K.kr(vp), res = Vector.empty, trjIdx = 0).unzip
  val freqIn = freqInSq: GE
  val ampIn  = ampInSq : GE
  
  // for each frequency, find the best past match
  val noFounds = (0 until numTraj).map { tIdx =>
    val fIn         = freqIn \ tIdx
    val aIn         = ampIn  \ tIdx
    val isOn        = aIn > 0
  
    val freqMatch   = (maxDf - (voiceFreq absdif fIn)).max(0)
    val bothOn      = voiceOnOff & isOn
    val bestIn      = 0 +: (freqMatch * (bothOn & !activated))
    val best        = ArrayMax.kr(bestIn)
    val bestIdx     = best.index - 1
  
    val bestMask    = voiceNos sig_== bestIdx
    activated      |= bestMask
    val bestMaskN   = !bestMask
    voiceFreq       = voiceFreq * bestMaskN + fIn * bestMask
    voiceAmp        = voiceAmp  * bestMaskN + aIn * bestMask
    
    bestIdx sig_== -1
  }
  
  for (tIdx <- 0 until numTraj) {
    val fIn             = freqIn \ tIdx
    val aIn             = ampIn  \ tIdx
    val isOn            = aIn > 0
    val voiceAvail      = !(activated | voiceOnOff)
  
    val notFound        = noFounds(tIdx)
    val startTraj       = notFound & isOn
    val free            = ArrayMax.kr(0 +: (startTraj & voiceAvail))
    val freeIdx         = free.index - 1
    val freeMask        = voiceNos sig_== freeIdx
    activated          |= freeMask
    val freeMaskN       = !freeMask
    voiceFreq           = voiceFreq * freeMaskN + fIn * freeMask
    voiceAmp            = voiceAmp  * freeMaskN + aIn * freeMask
  }
  
  // ---- voice generation ----
  val voiceEnv      = Env.asr(attack = egAtk, release = egRls)
  val voiceEG       = EnvGen.ar(voiceEnv, gate = activated)
  
  // ---- state out ----
  val voiceEGOn = A2K.kr(voiceEG) sig_!= 0
  voiceOnOff    = activated | voiceEGOn
  
  val stateOutKr  = voiceFreq ++ voiceAmp ++ voiceOnOff
  
  // ---- sound generation ----
  
  // gate so attack doesn't lag
  val lagTimeGt = (activated sig_== Delay1.kr(activated)) * lagTime
  val ampScale  = voiceAmp.linexp(0, 1, -20.dbamp, 0.dbamp)
  val ampLag    = Lag.ar(voiceAmp , time = lagTimeGt)
  val freqScale = voiceFreq.linexp(0, 1, minFreq, maxFreq)
  val freqLag   = Lag.ar(freqScale, time = lagTimeGt)
  val sines     = SinOsc.ar(freqLag) * voiceEG * ampLag
  val mix       = Mix(sines)
  (mix -> stateOutKr)
}

val (left , leftState)  = mkSide(true )
val (right, rightState) = mkSide(false)

LocalOut.kr(leftState ++ rightState)

val mix   = Limiter.ar(Seq(/* DC.ar(0) */ left, right))

output := mix
