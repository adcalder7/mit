package lessons.designpatterns.creational

object AnalysisUtil {
  val pi:Double = 3.14d

  // Won't be loaded in memory until after its called
  lazy val piExtended:Double = {
    println("Calculating PI_Extended... Heavy task... This message will only print once")
    3.14534534345345345345345d
  }
}

object LazyInitialization extends App {
  println(AnalysisUtil.pi * 2)
  println(AnalysisUtil.pi * 2)
  println(AnalysisUtil.pi * 2)

  println(AnalysisUtil.piExtended * 2)
  println(AnalysisUtil.piExtended * 2)
}
