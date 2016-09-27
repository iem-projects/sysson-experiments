val numBands = 200

config.audioBusChannels = (numBands * 2).nextPowerOfTwo
config.wireBuffers      = numBands * 4
boot()

val bus = Bus.audio(numChannels = numBands + 1) // one more for the player interp
val bTick = Bus.audio()

// generates a single trajectory
val gen = play {
  val res    = "resolution".kr(4)  // frequency
  val months = "months".kr(28)
  val triF   = res / (2 * months)
  val tri    = LFTri.ar(triF).linlin(-1, 1, 0, numBands - 1)
  val tick   = Impulse.ar(res)
  val freq   = Latch.ar(tri, tick)
  val freq0  = freq.floor
  val freq1  = freq0 + 1
  val w1     = freq % 1
  val w0     = 1 - w1
  Out.ar(bus.index + freq0, w0) // XXX problem with a-rate bus changes?
  Out.ar(bus.index + freq1, w1)
  Out.ar(bTick.index, tick)
}

bus.gui.meter()

val sonif = play(addAction = addToTail) {
  val res     = "resolution".kr(4)  // frequency
  val maxSpace= "max-space".kr(numBands / 4)
  val tick    = In.ar(bTick.index) //  Impulse.ar(res)
  val in      = In.ar(bus.index, numBands)
  val max     = ArrayMax.ar(in)
  val dlyVal  = max.value - Delay1.ar(max.value)
  val dlyIdx  = max.index - Delay1.ar(max.index)
  dlyIdx.poll(tick, "delta")
  // Select.ar(max.index, in)
  val freq    = max.index.linexp(0, numBands - 1, 200, 10000)
  val amp     = max.value /4
  SinOsc.ar(freq) * amp
}

sonif.free()

gui(addAction = addToTail) {
  val res       = "resolution".kr(4)  // frequency
  val maxSpace  = "max-space".kr(numBands / 4)
  val tick      = In.ar(bTick.index) //  Impulse.ar(res)
  val in        = In.ar(bus.index, numBands)
  val max       = ArrayMax.ar(in)
  val maxValDly = Delay1.ar(max.value)
  val maxIdxDly = Delay1.ar(max.index)
  val maxValCh  = max.value - maxValDly
  val valIdxCh  = max.index - maxIdxDly
  Seq(max.index / numBands, valIdxCh, tick)
//   dlyIdx.poll(tick, "delta")
//   // Select.ar(max.index, in)
//   val freq    = max.index.linexp(0, numBands - 1, 200, 10000)
//   val amp     = max.value /4
//   SinOsc.ar(freq) * amp
} .waveform(duration = 1)

////////////////////

gui(addAction = addToTail) {
  val pulse = LFPulse.ar(10, iphase = Seq(0, 0.5))
  val max       = ArrayMax.ar(pulse)
  Flatten(Seq(pulse, max.index))
} .waveform(duration = 1)


///////////////////////////////////////////////
// THE ABOVE FAILED BECAUSE Out.ar() DOESN'T //
// GIVE AUDIO-RATE TO THE BUS ARGUMENT       //
///////////////////////////////////////////////

// generates a single trajectory
val gen = play {
  val res    = "resolution".kr(4)  // frequency
  val months = "months".kr(28)
  val amp    = "amp".kr(1)
  val triF   = res / (2 * months)
//   val tri    = LFTri.ar(triF).linlin(-1, 1, 0, numBands - 1)
  val tri    = LFSaw.ar(triF, iphase = 1).linlin(1, -1, 0, numBands - 1)
  val tick   = Impulse.ar(res)
  val freq   = Latch.ar(tri, tick)
  val freq0  = freq.floor
  val freq1  = freq0 + 1
  val w1     = freq % 1
  val w0     = 1 - w1
  val a1     = w1 * amp
  val a0     = w0 * amp
  
  val indices = Seq[GE](0 until numBands): GE
  val sig = (indices sig_== freq0) * a0 + (indices sig_== freq1) * a1
  Out.ar(bus.index, sig)
  Out.ar(bTick.index, tick)
}

// bus.gui.meter()

val sonif = play(addAction = addToTail) {
  val res     = "resolution".kr(4)  // frequency
  val maxSpace= "max-space".kr(numBands / 4)
  val ampThresh= "amp-thresh".kr(0.5)
  val tick    = In.ar(bTick.index) //  Impulse.ar(res)
  val in      = In.ar(bus.index, numBands)
  val max     = ArrayMax.ar(in)
  val prevVal = Delay1.ar(max.value)
  val nextVal = max.value
  val dVal    = nextVal - prevVal
  val prevIdx = Delay1.ar(max.index)
  val nextIdx = max.index
  val dIdx    = nextIdx - prevIdx
  val nextOn  = nextVal > ampThresh
  val isTraj  = (dIdx.abs < maxSpace) | !nextOn
  
  DC.ar(0).poll(1 - isTraj, "NOT")

  val nextFreq = max.index.linexp(0, numBands - 1, 200, 10000)
  val prevFreq = Delay1.ar(nextFreq)
  
  val nextFreqL = Latch.ar(nextFreq, tick & nextOn)
  val prevFreqL = Latch.ar(prevFreq, tick)
  val envF = Env(prevFreqL, Env.Segment(1.0 / res, nextFreqL, Curve.linear /* sine */) :: Nil, loopNode = 0)
  val egF = EnvGen.ar(envF, gate = tick)
  
  val freq = egF

  val nextAmpL = Latch.ar(nextVal, tick)
  val prevAmpL = Latch.ar(prevVal, tick)
  val envA = Env(prevAmpL, Env.Segment(1.0 / res, nextAmpL, Curve.linear /* sine */) :: Nil, loopNode = 0)
  val egA = EnvGen.ar(envA, gate = tick)
  
  //  dIdx.poll(tick, "delta")
  // Select.ar(max.index, in)
  val amp     = egA /4
  SinOsc.ar(freq) * amp
}

gen.set("amp" -> 0)
gen.set("amp" -> 1)

// def plotCurve(c: Curve): Unit =
//    Vector.tabulate(100)(i => c.levelAt(i/99.0f, y1 = 0, y2 = 1)).plot(discrete = true)
// 
// plotCurve(Curve.step)
// plotCurve(Curve.welch)
// plotCurve(Curve.sine)

