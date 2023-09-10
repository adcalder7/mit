package lessons.designpatterns.structural

// Make incompatible things work
// Usually comes up when we can't edit the library

// Previous Version: App called logger.info, logger.warn, logger.debug, etc
class Logger_old() {
  def info(message: String): Unit = println(s"info:$message")
  def warn(message: String): Unit = println(s"warn:$message")
  def debug(message: String): Unit = println(s"debug:$message")
  def error(message: String): Unit = println(s"error:$message")
}

// New Version: App will break so we need an adapter
class Logger() {
  def log(message:String, severity: String) = {
    println(s"$severity:$message")
  }
}

package object LoggerAdapter {
  implicit class LoggerAdapter(logger:Logger) {
    def info(message: String): Unit = logger.log(message, "info")
    def warn(message: String): Unit = logger.log(message, "warn")
    def debug(message: String): Unit = logger.log(message, "debug")
    def error(message: String): Unit = logger.log(message, "error")
  }
}

object Driver extends App {
  import LoggerAdapter._
  val logger = new Logger()
  logger.info("error")
  logger.warn("error")
  logger.debug("error")
  logger.error("error")
}
