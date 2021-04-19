package interviews.rewardsnetwork.gd.engine

import interviews.rewardsnetwork.gd.context.{Command, Context, Rules}
import com.typesafe.scalalogging.LazyLogging

object Engine extends LazyLogging {

  def run(args: Array[String]): Unit = {
      try {
        // Validate starting arguments
        val argsCount = args.length
        if (argsCount == 0) throw new SilentException("No arguments provided")
        val cmdList = for (i <- 1 until args.length) yield args(i).trim.replaceAll(",", "")

        // Initialize context
        val context = Context(args(0))
        val ruleSet = getDefaultRuleSet()(context)

        // Execute commands
        for (i <- 0 until cmdList.length) {
          val (output, test) = Command.execute(cmdList(i))(context, ruleSet)
          if (!test) throw new SilentException(s"Failed with output $output")
          else {
            print(output)

            // Avoid comma at the end
            if (i < argsCount-2) print(", ")
            else if (i >= argsCount-2) println()
          }
        }
      } catch {
        case e: SilentException => {
          logger.debug(s"\n***Exception: ${e.getMessage}***")
          println("fail")
        }
      }
  }

  def getDefaultRuleSet()(implicit context: Context): Rules = {
    Rules(List(
      Rules.mustHavePJsOff _,
      Rules.onlyOneGearTypeAtATime _,
      Rules.noSocksOnHotDay _,
      Rules.noJacketWhenHot _,
      Rules.socksBeforeFootwear _,
      Rules.pantsBeforeFootwear _,
      Rules.shirtBeforeHeadGearOrJacket _),
      Map("leaveHouse" -> List(Rules.canLeaveHouse _)))
  }

}
