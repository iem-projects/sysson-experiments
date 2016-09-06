val x = gui {
  val lagFrames  = "lagFrames".ir(8)
  val lagDur     = lagFrames / SampleRate.ir
  val condAcc    = Phasor.ar(lo = 2, hi = 6)
  val condAccT   = DelayN.ar(condAcc, SampleDur.ir, SampleDur.ir)
  val condChange = condAccT sig_!= condAcc
  condAcc .poll(0, "cond-acc")
  condAccT.poll(0, "cond-accT")
  val condChHold = Trig1.ar(condChange, lagDur)
  val heldAcc    = Latch.ar(condAcc, condChHold)
  val lagDurS    = DelayN.ar(lagDur, lagDur, lagDur)
  val heldDly    = DelayN.ar(heldAcc, delayTime = lagDurS, maxDelayTime = lagDur)
  heldDly
  condChange
  condAccT
}

x.waveform(duration = 24/s.sampleRate)

val x = gui {
  val condAcc    = Phasor.ar(lo = 2, hi = 8)
  val n = 3
  // bloody floating point noise because SampleDur is so small
  val nDur     = (n + 0.5) * ControlDur.ir
  val dt       = DelayN.kr(nDur, nDur, nDur)
  val condAccT = DelayN.ar(condAcc, maxDelayTime = nDur, delayTime = dt)
//  val condAccT = DelayN.ar(condAcc, nDur, 0)
  condAccT
  DC.ar(1) * dt
}

x.waveform(duration = 24/(s.sampleRate/64))

val x = gui {
  val condAcc    = Phasor.ar(lo = 2, hi = 8)
  val n = 4
  // bloody floating point noise because SampleDur is so small
  val nDur     = (n + 0.6) * SampleDur.ir
  val condAcc0 = DelayN.ar(condAcc, maxDelayTime = nDur, delayTime = nDur)
  val init = Trig1.ar(1, nDur)
  val condAccT = /* condAcc0 + */ condAcc * init
//  val condAccT = DelayN.ar(condAcc, nDur, 0)
  condAccT
  init
}

x.waveform(duration = 120/(s.sampleRate))

val x = gui {
  val n         = 3
  val condAcc   = Phasor.ar(lo = 2, hi = 8)
  // bloody floating point noise because SampleDur is so small
  val nDur      = (n + 0.6) * SampleDur.ir
  val condAcc0  = DelayN.ar(condAcc, maxDelayTime = nDur, delayTime = nDur)
  val init      = Impulse.ar(0) // Trig1.ar(1, nDur)
  val condAccT  = condAcc0 + condAcc * init
  condAccT
}

x.waveform(duration = 24/(s.sampleRate))

///// test "trig at time zero"

// no functiona, thanks to Delay1 init
play {
  val cond      = DC.kr(3)
  val ch        = Delay1.kr(cond) sig_!= cond
  ch.poll(0 /* Impulse.kr(ControlRate.ir/2) */, "zero")
  val phase     = Phasor.kr(lo = 1, hi = 100)
  phase.poll(ch, "DANG!")
  ()
}

// no functiona, thanks to HPZ1 init
play {
  val cond      = DC.kr(3)
  val ch        = HPZ1.kr(cond) sig_!= 0
  ch.poll(0 /* Impulse.kr(ControlRate.ir/2) */, "zero")
  val phase     = Phasor.kr(lo = 1, hi = 100)
  phase.poll(ch, "DANG!")
  ()
}

// val x = gui {
//   val condAcc = Phasor.ar(lo = 2, hi = 8)
//   val hpz = HPZ1.ar(condAcc)
//   hpz
// }
// 
// x.waveform(duration = 24/(s.sampleRate))

// no functiona
play {
  val cond      = DC.kr(3)
  val ch        = OneZero.kr(cond, -0.5) sig_!= 0
  ch.poll(0 /* Impulse.kr(ControlRate.ir/2) */, "zero")
  val phase     = Phasor.kr(lo = 1, hi = 100)
  phase.poll(ch, "DANG!")
  ()
}

// I have no clue where the value of 2 at the
// beginning is supposed to come from.
val x = gui {
  val condAcc = Phasor.ar(lo = 2, hi = 8)
  val hpz = OneZero.ar(condAcc)
  hpz / 2
}

x.waveform(duration = 24/(s.sampleRate))

play {
  val cond      = DC.kr(3)
  val ch        = DelayN.kr(cond, 1.6 * ControlDur.ir, 1.6 * ControlDur.ir) sig_!= cond
  ch.poll(0, "zero")
  val phase     = Phasor.kr(lo = 1, hi = 100)
  phase.poll(ch, "DANG!")
  ()
}

//////////////////////////

val x = gui {
  val n         = 3
  val condAcc   = Phasor.ar(lo = 2, hi = 8)
  // bloody floating point noise because SampleDur is so small
  val nDur      = (n + 0.6) * SampleDur.ir
  val condAcc0  = DelayN.ar(condAcc, maxDelayTime = nDur, delayTime = nDur)
  val init      = /* Impulse.ar(0) */ Trig1.ar(1, nDur)
  val condAccT  = condAcc0 + condAcc * init
  condAccT
}

x.waveform(duration = 24/(s.sampleRate))

/////////////////////////////

play {
  val in = DC.kr(2)
  val tr = Impulse.kr(2) 
  val latch = Gate.kr(in, ToggleFF.kr(tr - Impulse.kr(0)))
  latch.poll(tr, "latch")
  ()
}

//////////////////////////

def k2a_SH(in: GE) = Latch.ar(in, Impulse.ar(ControlRate.ir))

def Impulse0LOL() = DC.ar(1) - Delay2.ar(DC.ar(1))

val x = gui {
    val condAcc: GE = "cond".kr(1)
    val n          = 1
    val lagDur     = (n + 0.1) * ControlDur.ir
    println(s"lagDur = $lagDur")
    val t0         = Impulse.kr(0)
    // val t0 = Impulse0LOL()
    val condChange = /* Delay1.kr(condAcc) sig_!= condAcc + */ t0
    val condChHold = Trig1.kr(condChange, lagDur)
//     val heldAcc    = Latch.kr(condAcc, condChHold)
//     val heldDly    = DelayN.kr(heldAcc, lagDur) + heldAcc * t0
//    DC.ar(1) * condChHold
    k2a_SH(condChHold)
    // K2A.ar(condChHold)
 }

x.waveform (duration = 64/(s.sampleRate/64))

64*5 == 320

////////////////////////////////////

val x = gui {
    // val condAcc: GE = "cond".kr(1)
    val condAcc: GE = Phasor.kr(lo = 1, hi = 100)
    val n          = 4
    val lagDur     = (n + 0.1) * ControlDur.ir
    val t0         = Impulse.kr(0)
    val condChange = Delay1.kr(condAcc) sig_!= condAcc + t0
    val condChHold = Trig1.kr(condChange, lagDur)
    val heldAcc    = Latch.kr(condAcc, condChHold)
//     val heldDly    = DelayN.kr(heldAcc, lagDur) + heldAcc * t0
//    DC.ar(1) * condChHold
    k2a_SH(heldAcc)
    // K2A.ar(condChHold)
 }

x.waveform (duration = 64/(s.sampleRate/64))

////////////////////////////////////

def k2a_SH(in: GE) = Latch.ar(in, Impulse.ar(ControlRate.ir))

gui {
    val condAcc: GE = Stepper.kr(DelayN.kr(Impulse.kr(ControlRate.ir/2), ControlDur.ir, ControlDur.ir), lo = 1, hi = 100)
    val t0         = Impulse.kr(0)
    val n          = 4
    val lagDur     = (n + 0.1) * ControlDur.ir
    val condChange = (Delay1.kr(condAcc) sig_!= condAcc) + t0
    val condChDly  = TDelay.kr(condChange, lagDur)
    val condChPunch= condChange // - condChDly
    val condChHold = SetResetFF.kr(condChPunch, condChDly)
    val heldAcc    = Latch.kr(condAcc, condChHold)
    val heldDly0   = DelayN.kr(heldAcc, lagDur, lagDur)
    val heldDly    = heldDly0 + heldAcc * (heldDly0 sig_== 0)
    k2a_SH(condAcc)
} .waveform (duration = 32/(s.sampleRate/64))

4*64*2

320+256

//////////////////////////

val x = play {
  val condAcc: GE = "acc".kr(1)
  val t0         = Impulse.kr(0)
  val n          = 4
  val lagDur     = (n + 0.1) * ControlDur.ir
  val condChange = (Delay1.kr(condAcc) sig_!= condAcc) + t0
  val condChDly  = TDelay.kr(condChange, lagDur)
  val condChPunch= condChange // - condChDly
  val condChHold = SetResetFF.kr(condChPunch, condChDly)
  val heldAcc    = Latch.kr(condAcc, condChHold)
  val heldDly0   = DelayN.kr(heldAcc, lagDur, lagDur)
  val heldDly    = heldDly0 + heldAcc * (heldDly0 sig_== 0)
  heldDly.poll(1, "out")
}

x.set("acc" -> 2)
x.set("acc" -> 3)
x.set("acc" -> 4)
x.set("acc" -> 5)
x.set("acc" -> 1)
x.set("acc" -> 0)
