package lessons.designpatterns.structural

// Provides an interface to something else that then gets served behind the scenes to the user
// Delegates expensive operations when needed

// lazy, flyweight, decorator designs combined

trait IExtractor {
  def extract
}

class Extractor extends IExtractor {
  println("2 hours on just initialization")
  override def extract: Unit = {
    println("1 hour more extracting")
  }
}

class ExtractorProxy extends IExtractor {
  lazy val extractor = new Extractor()
  override def extract: Unit = extractor.extract
}

object ProxyTest extends App {
  var extractors = List[IExtractor]()
  // Using the concrete class will cause things to run (like the constructor)
  println("Added concrete extractor to list: ")
  extractors = extractors :+ new Extractor()

  // Using the proxy will silently wait till extract is called
  println("Proxy added to list: ")
  extractors = extractors :+ new ExtractorProxy()
  println("Silence...")
}
