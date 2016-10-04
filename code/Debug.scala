object Debug {
   
}

case class Debug(in: GE, label: String = "debug") extends UGenSource.ZeroOut with ControlRated {
  import UGenSource._
  
  def makeUGens: Unit = unwrap(in.expand)
  
  def makeUGen(in: Vec[UGenIn]): Unit = {
    ???   
  }
}
