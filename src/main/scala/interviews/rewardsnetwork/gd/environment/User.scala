package gd.environment

import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer

// Assuming gear names are unique
class User() extends LazyLogging {
  private var _headGear:ListBuffer[HeadGear] = ListBuffer.empty
  private var _torsoGear:ListBuffer[TorsoGear] = ListBuffer.empty
  private var _legGear:ListBuffer[LegGear] = ListBuffer.empty
  private var _footGear:ListBuffer[FootGear] = ListBuffer.empty

  // Getters
  def getHeadGear():List[HeadGear] = List() ++ _headGear
  def getTorsoGear():List[TorsoGear] = List() ++ _torsoGear
  def getLegGear():List[LegGear] = List() ++ _legGear
  def getFootGear():List[FootGear] = List() ++ _footGear
  def getGear(): List[Gear] = List() ++ _headGear ++ _torsoGear ++ _legGear ++ _footGear

  def addGear(gear: Gear): Boolean = {
    gear match {
      case i: HeadGear => _headGear += i; true
      case i: TorsoGear =>  _torsoGear += i; true
      case i: LegGear => _legGear += i; true
      case i: FootGear => _footGear += i; true
      case _ => {
        logger.debug(s"\n***Invalid gear type ${gear.getClass.toString}***")
        false
      }
    }
  }

  def removeGear(gear: Gear): Option[Gear] = {
    gear match {
      case hg: HeadGear => _headGear -= hg; Some(gear)
      case tg: TorsoGear => _torsoGear -= tg; Some(gear)
      case lg: LegGear => _legGear -= lg; Some(gear)
      case fg: FootGear => _footGear -= fg; Some(gear)
      case _ => {
        logger.debug(s"\n***Gear ${gear.name} not found on user***")
        None
      }
    }
  }

  def removeGear(name: String): Option[Gear] = {
    val gear = getGear find(gear => gear.name.equals(name))
    if (gear.isDefined) removeGear(gear.get)
    else logger.debug(s"\n***Gear ${name} not found on user***")
    gear
  }

  def removeGearSet(name: String): List[Gear] = {
    val set = getGear filter(gear => gear.set.equals(name))
    set foreach removeGear

    if (set.isEmpty) logger.debug(s"\n***Gear set ${name} not found on user***")

    set
  }

}