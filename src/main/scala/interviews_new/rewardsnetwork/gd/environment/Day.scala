package interviews_new.rewardsnetwork.gd.environment

import com.typesafe.scalalogging.LazyLogging
import interviews_new.rewardsnetwork.gd.engine.SilentException

case class Day private(temperature: String) { }

object Day extends LazyLogging {
  def apply(temperature: String):Day = {
    temperature match {
      case "HOT" => new Day(temperature)
      case "COLD" => new Day(temperature)
      case _ => throw new SilentException(s"Invalid day temperature ${temperature}")
    }
  }
}
