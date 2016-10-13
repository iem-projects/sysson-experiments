// val xs = Vector.tabulate(100)(i => if (i > 50) 0 else 1)
// xs.zipWithIndex.map { case (x, i) => x * i } .sum / xs.sum
// 
// def sinc(x: Double) = if (x == 0) 1 else math.sin(x)/x
// 
// (-10 to 10).map(x => sinc(x * 1.5)).plot(discrete = true)

val xs = (-10 to 10).map(x => if (x == 0) 1 else math.sin(x) / (x.abs.pow(0.5) * x.signum))
xs.plot(discrete = true)
xs.map(x => f"$x%1.2f").mkString(", ")

val ys = Vector(0.17, 0.14, 0.35, 0.25, -0.11, -0.43, -0.38, 0.08, 0.64, 0.84, 1.00, 0.84, 0.64, 0.08, -0.38, -0.43, -0.11, 0.25, 0.35, 0.14, -0.17)
ys.plot(discrete = true)

val bestVal   = ys.max               // 1.0
val bestIdx   = ys.indexOf(bestVal)  // 10
val thresh    = 0.2
val yi        = ys.indices

val beforeIdx = bestIdx - ys.take(bestIdx).reverse.indexWhere(_ < thresh) // 8
val afterIdx  = bestIdx + ys.drop(bestIdx + 1).indexWhere(_ < thresh) // 12
val mask      = yi.map(i => if (i >= beforeIdx && i <= afterIdx) 1 else 0)
val inMask    = (ys zip mask).map { case (y, m) => y * m }
val inSum     = inMask.sum // 3.96
val wIdx      = (inMask.zipWithIndex).map { case (y, i) => y * i } .sum / inSum // 10

val isInvalid = ys.map(y => if (y < thresh) 1 else 0)
// in Scala, NaN times zero is NaN
val before    = isInvalid.zip(yi).map { case (b, i) => if (i == bestIdx) 0 else (bestIdx - i).reciprocal * b }
//  Vector(1/10,0, 1/9.0, 0.0, 0.0, 1/6.0, 1/5.0, 1/4.0, 1/3.0, 0.0, 0.0, 0.0, 
//         0.0, 0.0, -1/3.0, -1/4.0, -1/5.0, -1/6.0, 0.0, 0.0, -1/9.0, -1/10.0)
val beforeIdx = before.indexOf(before.max) + 1 // 8
val after     = isInvalid.zip(yi).map { case (b, i) => if (i == bestIdx) 0 else (i - bestIdx).reciprocal * b }
val afterIdx0 = after.indexOf(after.max)
val afterIdx  = (if (afterIdx0 > 0) afterIdx0 else ys.size) - 1   // 12
val mask      = yi.map(i => if (i >= beforeIdx && i <= afterIdx) 1 else 0)
val inMask    = (ys zip mask).map { case (y, m) => y * m }
val inSum     = inMask.sum // 3.96
val wIdx      = (inMask.zipWithIndex).map { case (y, i) => y * i } .sum / inSum // 10

implicit class MyGEOps(private val in: GE) /* extends AnyVal */ {
  def +: (head: Constant): GE = Flatten(Seq(head, in))
  def :+ (last: Constant): GE = Flatten(Seq(in, last))
  def ++ (that: GE      ): GE = Flatten(Seq(in, that))
  def \  (r: Range      ): GE = r.map(i => in \ i): GE
}

def test(ys0: Vector[Double]): Unit = {
  val ys: GE    = ys0.map(y => y: GE)
  val best      = ArrayMax.kr(ys)
  val bestVal   = best.value
  val bestIdx   = best.index
  val yi        = ChannelIndices(ys)
  val isInvalid = ys < thresh
  val bestDist  = (bestIdx - yi).reciprocal
  val before    = ArrayMax.kr(0 +: (bestDist * isInvalid))
  val beforeIdx = before.index
  beforeIdx.poll(0, "beforeIdx")
  val after     = ArrayMax.kr(0 +: (-bestDist * isInvalid))
//   ((-bestDist * isInvalid) :+ 0).poll(0, "FOO")
  val afterIdx  = (after.index - 1).wrap(0, NumChannels(ys) + 1)
  afterIdx.poll(0, "afterIdx")
  val mask      = yi >= beforeIdx & yi < afterIdx
  val inMask    = ys * mask
  val inSum     = Reduce.+(inMask)  // 3.96
  inSum.poll(0, "inSum")
  val wIdx      = Reduce.+(inMask * yi) / inSum  // 0 / 0 == 0 in scsynth
  wIdx.poll(0, "wIdx")
}

play(test(ys))  // inSum: 3.96, wIdx: 10

play(test(ys.drop(8)))

play(test(ys.take(13)))

play(test(ys.slice(8, 13)))

play(test(Vector.fill(4)(0.0)))

// in scsynth, NaN times zero is zero
play {
  (DC.kr(0) / DC.kr(0) * 0).poll(0, "NaN * 0")
  ()
}

// ---- cold anomalies

def test(ys0: Vector[Double]): Unit = {
  val ys: GE    = ys0.map(y => y: GE)
  val best      = ArrayMin.kr(ys)
  val bestVal   = best.value
  val bestIdx   = best.index
  val yi        = ChannelIndices(ys)
  val isInvalid = ys > -thresh
  val bestDist  = (bestIdx - yi).reciprocal
//  ((-bestDist * isInvalid) :+ 1).poll(0, "bestDist")
  val before    = ArrayMax.kr(0 +: (bestDist * isInvalid))
  val beforeIdx = before.index
  beforeIdx.poll(0, "beforeIdx")
  val after     = ArrayMax.kr(0 +: (-bestDist * isInvalid))
  val afterIdx  = (after.index - 1).wrap(0, NumChannels(ys) + 1)
  afterIdx.poll(0, "afterIdx")
  val mask      = yi >= beforeIdx & yi < afterIdx
  val inMask    = ys * mask
  val inSum     = Reduce.+(inMask)  // 3.96
  inSum.poll(0, "inSum")
  val wIdx      = Reduce.+(inMask * yi) / inSum  // 0 / 0 == 0 in scsynth
  wIdx.poll(0, "wIdx")
}

val zs = ys.map(-_)
zs.plot(discrete = true)

play(test(zs))  // inSum: -3.96, wIdx: 10

play(test(zs.drop(8)))

play(test(zs.take(13)))

play(test(zs.slice(8, 13)))

play(test(Vector.fill(4)(0.0)))

/////////////////////////////////

def test(ys0: Vector[Double], isUp: Boolean): Unit = {
  val ys: GE    = ys0.map(y => y: GE)
  val (bestVal, bestIdx) = if (isUp) {
    val best = ArrayMax.kr(ys)
    (best.value -> best.index)
  } else {
    val best = ArrayMin.kr(ys)
    (best.value -> best.index)
  }
  val yi        = ChannelIndices(ys)
  val isInvalid = if (isUp) ys < thresh else ys > -thresh
  val bestDist  = (bestIdx - yi).reciprocal * isInvalid
  val before    = ArrayMax.kr(0 +: bestDist)
  val beforeIdx = before.index
  beforeIdx.poll(0, "beforeIdx")
  val after     = ArrayMax.kr(0 +: -bestDist)
//   ((-bestDist * isInvalid) :+ 0).poll(0, "FOO")
  val afterIdx  = (after.index - 1).wrap(0, NumChannels(ys) + 1)
  afterIdx.poll(0, "afterIdx")
  val mask      = yi >= beforeIdx & yi < afterIdx
  val inMask    = ys * mask
  val inSum     = Reduce.+(inMask)  // 3.96
  inSum.poll(0, "inSum")
  val wIdx      = Reduce.+(inMask * yi) / inSum  // 0 / 0 == 0 in scsynth
  wIdx.poll(0, "wIdx")
}

play(test(ys, isUp = true))  // inSum: 3.96, wIdx: 10
play(test(ys.drop(8), isUp = true))
play(test(ys.take(13), isUp = true))
play(test(ys.slice(8, 13), isUp = true))
play(test(Vector.fill(4)(0.0), isUp = true))

play(test(zs, isUp = false))  // inSum: -3.96, wIdx: 10
play(test(zs.drop(8), isUp = false))
play(test(zs.take(13), isUp = false))
play(test(zs.slice(8, 13), isUp = false))
play(test(Vector.fill(4)(0.0), isUp = false))

