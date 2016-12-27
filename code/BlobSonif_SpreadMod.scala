// version: 27-Oct-2016

// blob vector:
// [0: id , 1; left  , 2: top , 3: width , 4: height,
//  5 - 9 = slice: 
//  5: top, 6: height, 7: mean, 8: stddev, 9: center] * numTraj
//
// numTraj, number of trajectories = 4

implicit class MyGEOps(private val in: GE) /* extends AnyVal */ {
  def +: (head: Constant): GE = Flatten(Seq(head, in))
  def :+ (last: Constant): GE = Flatten(Seq(in, last))
  def ++ (that: GE      ): GE = Flatten(Seq(in, that))
  def \  (r: Range      ): GE = r.map(i => in \ i): GE
}

// ---- matrix ----

val vr    = Var("anom-blobs")
val dTime = Dim(vr, "time")
val dAlt  = Dim(vr, "blobs")

val speed = UserValue.kr("speed", 6)
val tp    = dTime.play(speed)
val timeTr= Impulse.ar(speed)
val vp    = vr.play(tp, interp = 1 /* 2 */)

// ---- configuration ----

// maximum number of trajectories followed
val numTraj       = 4

// maximum number of concurrently playing voices
val numVoices     = numTraj * 2

// // maximum jump in altitude (normalized - full range equals one) before trajectory is interrupted
// val maxDf         = UserValue.kr("traj-max-dif [0-1]", 0.25)

// trajectory amplitude envelope attack duration
val egAtk         = UserValue.kr("traj-atk [s]", 0.2)

// trajectory amplitude envelope release duration
val egRls         = UserValue.kr("traj-rls [s]", 0.5)

// trajectory frequency and amplitude smear time
val lagTime       = 1.0

// // threshold above which temperatures are considered anormal
// val magThresh     = UserValue.kr("mag-thresh [°]", 1.5)

// maximum magnitude considered for anormal values
val magMax        = UserValue.kr("mag-max [°]", 2.5)

// sonification minimum oscillator frequency for hot anomalies
val minFreqH      = UserValue.kr("min-freq hot [Hz]", 300)

// sonification maximum oscillator frequency for hot anomalies
val maxFreqH      = UserValue.kr("max-freq hot [Hz]", 600)

// // sonification minimum oscillator frequency for cold anomalies
val minFreqC      = UserValue.kr("min-freq cold [Hz]", 1000)
// val minFreqC = 1000: GE
// 
// // sonification maximum oscillator frequency for cold anomalies
val maxFreqC      = UserValue.kr("max-freq cold [Hz]", 2000)
// val maxFreqC = 2000: GE

val modFreq       = UserValue.kr("spread mod freq  [Hz]", 8)
val modDepth      = UserValue.kr("spread mod depth", 1.0)
val modOff        = UserValue.kr("spread mod offset", 0.1)

// sonification oscillator amount of resonance
// val filterQ       = UserValue.kr("filter Q (1 to 100)", 20)
// val filterRQ      = filterQ.reciprocal

// time grid indicator volume
val gridAmp       = UserValue.kr("time grid [dB]", -18).dbamp

// ---- state ----

// val stateInKr     = LocalIn.kr(Seq.fill(numVoices * 4 * 2)(0))
val numFeatures   = 5
val stateInKr     = LocalIn.kr(Seq.fill(numVoices * numFeatures * 2)(0))

// ---- time grid ----

def mkGrid(): GE = {
  // XXX TODO --- we need an operator
  // that can automatically decode the dates,
  // since other data sets will use other scaling
  val seconds      = tp
  val daysPerYear  = 365.2422  // average according to NASA
  val daysPerMonth = daysPerYear / 12
  val secsPerMonth = daysPerMonth * 24 * 60 * 60
  val months       = seconds / secsPerMonth
  val month        = months.floor % 12 // + 1
  val isJan        = month sig_== 0
  val isJul        = month sig_== 6
  
  val monthPulse  = timeTr
  val gridDecTime = speed.reciprocal.min(0.5)
  val gridDecay   = Decay.ar(monthPulse, gridDecTime)
  val gridBase    = WhiteNoise.ar(gridDecay) * gridAmp
  val gridJan     = Resonz.ar(gridBase, 1000, rq = 1)
  val gridJul     = Resonz.ar(gridBase, 3500, rq = 0.5) * 1.5 // * LFPulse.ar(40)
  val gridPlain0  = Resonz.ar(gridBase, 2000, rq = 2) * 0.25
  val gridPlain   = Select.ar(speed <= 6, Seq(DC.ar(0), gridPlain0))
  val gridIdx     = isJan | (isJul << 1)
  val gridSig     = Select.ar(index = gridIdx, in = Seq(gridPlain, gridJan, gridJul))
  
  Pan2.ar(gridSig)
}

// ---- matrix analysis ----

val voiceNos      = 0 until numVoices: GE
val vpChanIdx     = ChannelIndices(vp)

def mkSide(isUp: Boolean): (GE, GE) = {
  // ---- state ----
  val stateOff      = if (isUp) 0 else numVoices * numFeatures
  var voiceId       = stateInKr \ ((numVoices * (stateOff+0)) until (numVoices * (stateOff+1)))
  var voiceFreq     = stateInKr \ ((numVoices * (stateOff+1)) until (numVoices * (stateOff+2)))
  var voiceAmp      = stateInKr \ ((numVoices * (stateOff+2)) until (numVoices * (stateOff+3)))
  var voiceOnOff    = stateInKr \ ((numVoices * (stateOff+3)) until (numVoices * (stateOff+4)))
  var voiceSpread   = stateInKr \ ((numVoices * (stateOff+4)) until (numVoices * (stateOff+5)))
  
  // ---- trace trajectories ----
  
  def extract(in: GE, res: Seq[(GE, GE, GE, GE)], tIdx: Int): Seq[(GE, GE, GE, GE)] = 
    if (tIdx == numTraj) res else {
// [0: id , 1; left  , 2: top , 3: width , 4: height,
//  5 - 9 = slice: 
//  5: top, 6: height, 7: mean, 8: stddev, 9: center] * numTraj

      val blobOff = tIdx * 10
      val id0     = in \ (blobOff + 0)
      val freq0   = in \ (blobOff + 9)
      val amp0    = in \ (blobOff + 7)
      val spread0 = in \ (blobOff + 6)
      
//       if (trjIdx == 0) {
//         val trF = 4
//         id0  .poll(trF, "id  -0")
//         freq0.poll(trF, "freq-0")
//         amp0 .poll(trF, "amp -0")
//       }
      val freqOk: GE  = CheckBadValues.kr(freq0, post = 0) sig_== 0
      val id      = id0 * freqOk
      val idOk    = id > 0
      val freq1   = Gate.kr(freq0, freqOk)     
      val ampOk   = freqOk & idOk
      
      val freq    = freq1.clip(0, 150   ).linlin(0, 150   , 0, 1)
      val amp     = amp0 .clip(0, magMax).linlin(0, magMax, 0, 1) * ampOk
      val spread  = spread0.clip(0, 150).linlin(0, 150   , 0, 1)

//       if (trjIdx == 0) {
//         val trF = 4
//         id   .poll(trF, "id  -1")
//         freq .poll(trF, "freq-1")
//         amp  .poll(trF, "amp -1")
//       }

      extract(in = in, res = res :+ ((id, freq, amp, spread)), tIdx = tIdx + 1)
    }
  
  var activated   = Vector.fill(numVoices)(0: GE): GE
  
  val XXX = extract(in = A2K.kr(vp), res = Vector.empty, tIdx = 0)
//   val (idInSq, freqInSq, ampInSq, spreadInSq) = extract(in = A2K.kr(vp), res = Vector.empty, tIdx = 0).unzip4
//   val identIn = idInSq  : GE
//   val freqIn  = freqInSq: GE
//   val ampIn   = ampInSq : GE
//   val spreadIn = spreadInSq : GE

  val identIn = XXX.map(_._1)  : GE
  val freqIn  = XXX.map(_._2): GE
  val ampIn   = XXX.map(_._3) : GE
  val spreadIn = XXX.map(_._4) : GE
  
  // for each frequency, find the best past match
  val noFounds = (0 until numTraj).map { tIdx =>
    val idIn        = identIn \ tIdx
    val fIn         = freqIn  \ tIdx
    val aIn         = ampIn   \ tIdx
    val spIn            = spreadIn \ tIdx
    val isOn        = idIn > 0
  
//     if (tIdx == 0) {
//       val trF = 4
//       idIn.poll(trF, "id  -2")
//       fIn .poll(trF, "freq-2")
//       aIn .poll(trF, "amp -2")
//     }
//  
     val idMatch     = voiceId sig_== idIn
//     val freqMatch   = (maxDf - (voiceFreq absdif fIn)).max(0)
    val bothOn      = voiceOnOff & isOn
    val bestIn      = 0 +: (idMatch * (bothOn & !activated))
    val best        = ArrayMax.kr(bestIn)
    val bestIdx     = best.index - 1
  
    val bestMask    = voiceNos sig_== bestIdx
    activated      |= bestMask
    val bestMaskN   = !bestMask
    voiceId         = voiceId     * bestMaskN + idIn * bestMask
    voiceFreq       = voiceFreq   * bestMaskN + fIn  * bestMask
    voiceAmp        = voiceAmp    * bestMaskN + aIn  * bestMask
    voiceSpread     = voiceSpread * bestMaskN + spIn * bestMask
    
    bestIdx sig_== -1
  }
  
//   activated.poll(0, "acti-0")
  
  for (tIdx <- 0 until numTraj) {
    val idIn            = identIn \ tIdx
    val fIn             = freqIn  \ tIdx
    val aIn             = ampIn   \ tIdx
    val spIn            = spreadIn \ tIdx
    val isOn            = idIn > 0
    val voiceAvail      = !(activated | voiceOnOff)
  
//     if (tIdx == 0) {
//       val trF = 4
//       idIn.poll(trF, "id  -3")
//       fIn .poll(trF, "freq-3")
//       aIn .poll(trF, "amp -3")
//     }
//   
    val notFound        = noFounds(tIdx)
    val startTraj       = notFound & isOn
    val free            = ArrayMax.kr(0 +: (startTraj & voiceAvail))
    val freeIdx         = free.index - 1
    val freeMask        = voiceNos sig_== freeIdx
    activated          |= freeMask
    val freeMaskN       = !freeMask
    voiceId             = voiceId     * freeMaskN + idIn * freeMask
    voiceFreq           = voiceFreq   * freeMaskN + fIn  * freeMask
    voiceAmp            = voiceAmp    * freeMaskN + aIn  * freeMask
    voiceSpread         = voiceSpread * freeMaskN + spIn * freeMask
  }

//   activated.poll(0, "acti-1")
    
  // ---- voice generation ----
  val voiceEnv      = Env.asr(attack = egAtk, release = egRls)
  val voiceEG       = EnvGen.ar(voiceEnv, gate = activated)
  
  // ---- state out ----
  val voiceEGOn = A2K.kr(voiceEG) sig_!= 0
  voiceOnOff    = activated | voiceEGOn
  
  val stateOutKr  = voiceId ++ voiceFreq ++ voiceAmp ++ voiceOnOff ++ voiceSpread

//   voiceFreq.poll(0, "vc-freq")
//   voiceAmp .poll(0, "vc-amp ")
  
  // ---- sound generation ----
  
  // gate so attack doesn't lag
  val lagTimeGt = (activated sig_== Delay1.kr(activated)) * lagTime
  val ampScale  = voiceAmp.linexp(0, 1, -20.dbamp, 0.dbamp)
  val ampLag    = Lag.ar(voiceAmp , time = lagTimeGt)
  val minFreq   = if (isUp) minFreqH else minFreqC
  val maxFreq   = if (isUp) maxFreqH else maxFreqC
  val freqScale = voiceFreq.linexp(0, 1, minFreq, maxFreq)
  val spreadScale = (voiceSpread - modOff).max(0).linlin(0, 1 - modOff, 0, modDepth)
  val freqLag   = Lag.ar(freqScale, time = lagTimeGt)
  val osc       = if (isUp) {
    val spreadLag = Lag.ar(spreadScale, time = lagTimeGt)
    SinOsc.ar(freqLag * (1 + spreadLag * SinOsc.ar(modFreq)))
    } else {
      SinOsc.ar(freqLag)
//       Resonz.ar(Dust2.ar(400), freqLag, rq = filterRQ) * 10
    }
  val sines     = osc * voiceEG * ampLag
  val mix       = Mix(sines)
  (mix -> stateOutKr)
}

val (left , leftState)  = mkSide(true )
// val (right, rightState) = mkSide(false)
val right      = DC.ar(0)
val rightState = Seq.fill(numVoices * numFeatures)(0): GE

LocalOut.kr(leftState ++ rightState)

val grid  = mkGrid()
val sig   = (Seq(left, right): GE) + grid
// val sig   = Pan2.ar(left) + grid
val mix   = Limiter.ar(sig)

// mix.poll(4, "mix")

Elapsed := tp
output  := mix
