package interviews_new.rewardsnetwork.gd.context

import com.typesafe.scalalogging.LazyLogging

object Command extends LazyLogging {

  def execute(cmd:String)(implicit context: Context, rules: Rules):(String, Boolean) = {
    cmd match {
      case "1" => putOn("sandals")
      case "2" =>
        if (context.day.temperature.equals("COLD")) putOn("hat")
        else putOn("sunglasses")
      case "3" => putOn("socks")
      case "4" => putOn("shirt")
      case "5" => putOn("jacket")
      case "6" =>
        if (context.day.temperature.equals("COLD")) putOn("pants")
        else putOn("shorts")
      case "7" => leaveHouse()
      case "8" => takeOffSet("PJs")
      case _ => (s"Invalid command $cmd", false)
    }
  }

  private def execute(name:String, command: => (String, Boolean))
                     (implicit context: Context, rules: Rules):(String, Boolean) = {
    val (output, checkCmd) = command

    // I chose to execute all the rules even if 1 fails
    // Could have stopped on first fail
    val checkCtxRules:Boolean = rules.contextRules.foldLeft[Boolean](true)((e, j) => {e && j()})
    val checkCmdRules:Boolean = if (rules.commandRules.contains(name)) {
      rules.commandRules(name).foldLeft[Boolean](true)((e, j) => {e && j()})
    } else true

    val result = checkCmd && checkCtxRules && checkCmdRules

    (output, result)
  }

  private def putOn(gearName: String)(implicit context: Context, rules: Rules):(String, Boolean) = {
    execute ("putOn", {
      // Remove from dresser
      val gear = context.house.dresser.removeGear(gearName)._1

      // Put on user
      if (gear.isDefined) {
        context.house.user.addGear(gear.get)

        (gearName, true)
      } else ("", false)
    })
  }

  // Note: not used... but added for completion
  private def takeOff(gearName: String)(implicit context: Context, rules: Rules):(String, Boolean) = {
    execute("takeOff", {
      // Remove from user
      val gear = context.house.user.removeGear(gearName)

      // Put on dresser
      if (gear.isDefined) {
        context.house.dresser.addGear(gear.get)
        (gearName, true)
      } else ("", false)
    })
  }

  private def takeOffSet(setName: String)(implicit context: Context, rules: Rules):(String, Boolean) = {
    execute("takeOffSet", {
      // Remove from user
      val gear = context.house.user.removeGearSet(setName)

      // Put on dresser
      if (!gear.isEmpty) {gear.foreach(context.house.dresser.addGear)
        (s"Removing $setName", true)
      } else ("", false)
    })
  }

  private def leaveHouse()(implicit context: Context, rules: Rules):(String, Boolean) = {
    execute("leaveHouse", {
      // Remove user from house
      if (context.house.user != null) {

        // Can remove user from House in further implementations
        // Will have to make rules engine smarter to not execute user rules on an empty house

        ("leaving house", true)
      } else ("", false)
    })
  }

}
