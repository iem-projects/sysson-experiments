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
    val condAcc: GE = DC.ar(3)
    val t0         = Impulse.ar(0)
    val condChange = (Delay1.ar(condAcc) sig_!= condAcc) + t0
    val n = 4
    val lagDur = (n + 0.6) * SampleDur.ir // ControlDur.ir
    val foo = Phasor.ar(lo = 2, hi = 8)
    val condChHold = Trig1.ar(foo /* condChange */, lagDur)
    condChHold
}

x.waveform (duration = 32/(s.sampleRate))

/////////////////////////

def test(sig: => GE) =
  play {
    val n          = 4
    val lagDur     = (n + 0.3) * SampleDur.ir
    val condChHold = Trig1.ar(sig, lagDur)
    val down = HPZ1.ar(condChHold) < 0
    val count = Phasor.ar(lo = 1, hi = 1000)
    count.poll(down, "frames")
    FreeSelf.kr(T2K.kr(ToggleFF.ar(down)))
    ()
  }

test(Phasor.ar(lo = 2, hi = 8))  // 4 -- correct
test(Impulse.ar(0))  // 5 -- wrong!

/////////////////////////

def test(sig: => GE) =
  play {
    val n          = 4
    val lagDur     = (n + 0.1) * ControlDur.ir
    val condChHold = Trig1.kr(sig, lagDur)
    val down = HPZ1.kr(condChHold) < 0
    val count = Phasor.kr(lo = 1, hi = 1000)
    count.poll(down, "frames")
    FreeSelf.kr(ToggleFF.kr(down))
    ()
  }

test(Phasor.kr(lo = 2, hi = 8)) // 4 -- correct
test(Impulse.kr(0)) // 5 -- wrong!
test(DC.kr(1)) // 4 -- correct
test("freq".kr(1) > 0) // 4 -- correct
test(1) // 4 -- correct
test(DC.kr(1) - Delay2.kr(DC.kr(1)))

val x = gui {
  DC.ar(1) - Delay2.ar(DC.ar(1))
}

x.waveform(duration = 12/(s.sampleRate))
