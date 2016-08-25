SynthDef.recv("branch") {
  // TrigControl doesn't work with n_map
  val trig = "cond".kr // .tr
  val amp  = "amp".kr(1)
  val freq = TExpRand.kr(lo = 400, hi = 800, trig = trig)
  freq.poll(trig, "new freq")
  Out.ar(0, Pan2.ar(SinOsc.ar(freq) * amp))
}

SynthDef.recv("main") {
  val amp = "amp".kr(1)
  val branch = "branch".ir
  val condBus = "cond".ir
  val cond = amp > 0.0
  val trig = Trig.kr(cond, dur = ControlDur.ir)
  Out.kr(condBus, trig)
  Pause.kr(gate = cond, node = branch)
}

val g       = Group(s)
val sMain   = Synth(s)
val sBranch = Synth(s)
val busCond = Bus.control(s)

def create() = {
  val bndl = osc.Bundle.now(
    g.newMsg(target = s.defaultGroup, addAction = addToHead),
    sMain  .newMsg("main"  , target = g, args = List("amp" -> 0.0, "branch" -> sBranch.id, "cond" -> busCond.index)),
    sBranch.newMsg("branch", target = g, args = List("amp" -> 0.0)),
    sBranch.mapMsg("cond" -> busCond.index)
  )
  s ! bndl
}

create()

g.set("amp" -> 0.2)
g.set("amp" -> 0.0)
g.set("amp" -> 0.2)
g.set("amp" -> 0.0)
