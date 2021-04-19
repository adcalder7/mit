package gd.context

import com.typesafe.scalalogging.LazyLogging

case class Rules(contextRules: List[() => Boolean],
                 commandRules: Map[String, List[() => Boolean]] = Map.empty)

object Rules extends LazyLogging {

  // Pajamas must be taken off before anything else can be put on
  def mustHavePJsOff()(implicit context:Context):Boolean = {
    val result = !context.house.user.getGear().find(_.set == "PJs").isDefined

    if (!result) logger.debug(s"\n*****Rule mustHavePJsOff did not pass*****")

    result
  }

  // Only 1 piece of each type of clothing may be put on
  def onlyOneGearTypeAtATime()(implicit context:Context):Boolean = {
    val headGearCount = context.house.user.getHeadGear().size
    val torsoGearCount = context.house.user.getTorsoGear().size
    val legGearCount = context.house.user.getLegGear().size
    val footGearCount = context.house.user.getFootGear().size

    // Can be broken if we add different torso gear/foot gear
    // E.g 2 shirts on a COLD day will be okay
    // E.g 2 socks on a COLD day will be okay
    // Can be improved by further classifying gear
    val result = headGearCount < 2 && legGearCount < 2 && torsoGearCount < 3 && footGearCount < 3

    if (!result) logger.debug(s"\n*****Rule onlyOneGearTypeAtATime did not pass*****")

    result
  }

  // You cannot put on socks when it is hot
  def noSocksOnHotDay()(implicit context:Context):Boolean = {
    val result = !(context.day.temperature.equals("HOT") &&
      context.house.user.getGear.find(e => e.name.equals("socks")).isDefined)

    if (!result) logger.debug(s"\n*****Rule noSocksOnHotDay did not pass*****")

    result
  }

  // You cannot put on a jacket when it is hot
  def noJacketWhenHot()(implicit context:Context):Boolean = {
    val result = !(context.day.temperature.equals("HOT") &&
      context.house.user.getGear.find(e => e.name.equals("jacket")).isDefined)

    if (!result) logger.debug(s"\n*****Rule noJacketWhenHot did not pass*****")

    result
  }

  // Socks must be put on before footwear
  def socksBeforeFootwear()(implicit context:Context):Boolean = {
    val footGear = context.house.user.getFootGear()

    val result = if (footGear.size > 1)
      footGear.head.name.equals("socks")
    else true

    if (!result) logger.debug(s"\n*****Rule socksBeforeFootwear did not pass*****")

    result
  }

  // Pants must be put on before footwear
  def pantsBeforeFootwear()(implicit context:Context):Boolean = {
    val legGearCount = context.house.user.getLegGear().size
    val footGearCount = context.house.user.getFootGear().size

    val result = !(legGearCount == 0 && footGearCount > 0)

    if (!result) logger.debug(s"\n*****Rule pantsBeforeFootwear did not pass*****")

    result
  }

  // The shirt must be put on before the head-wear or jacket
  def shirtBeforeHeadGearOrJacket()(implicit context:Context):Boolean = {
    val headGearCount = context.house.user.getHeadGear().size
    val torsoGear = context.house.user.getTorsoGear()

    // Check
    val hasNothingOn = headGearCount == 0 && torsoGear.size == 0
    val hasHeadGearBeforeTorso = headGearCount != 0 && torsoGear.size == 0
    val hasJacketBeforeShirt = torsoGear.size != 0 && !torsoGear.head.name.equals("shirt")

    val result = hasNothingOn || !(hasHeadGearBeforeTorso || hasJacketBeforeShirt)

    if (!result) logger.debug(s"\n*****Rule shirtBeforeHeadGearOrJacket did not pass*****")

    result
  }

  // You cannot leave the house until all items of clothing are on (except socks and a jacket when it’s hot)
  def canLeaveHouse()(implicit context:Context):Boolean = {
    val headGearCount = context.house.user.getHeadGear().size
    val torsoGear = context.house.user.getTorsoGear()
    val legGearCount = context.house.user.getLegGear().size
    val footGear = context.house.user.getFootGear()

    val result2 = if (context.day.temperature.equals("COLD")) {
      footGear.size > 1 && footGear.head.name.equals("socks") && !footGear.last.name.equals("socks")
    } else footGear.size > 0

    val result3 = if (context.day.temperature.equals("COLD")) {
      torsoGear.size > 1 && torsoGear.last.name.equals("jacket")
    } else torsoGear.size > 0

    val result = headGearCount != 0 && legGearCount != 0 && result2 && result3

    if (!result) logger.debug(s"\n*****Rule canLeaveHouse did not pass*****")

    result
  }

}

