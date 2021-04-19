package interviews.rewardsnetwork.gd.environment

import com.typesafe.scalalogging.LazyLogging

trait Gear {
  // Assuming gear names are unique
  val name:String
  val set:String
}

case class HeadGear(name:String, set:String = "") extends Gear
case class TorsoGear(name:String, set:String = "") extends Gear
case class LegGear(name:String, set:String = "") extends Gear
case class FootGear(name:String, set:String = "") extends Gear

class Dresser private() extends LazyLogging {
  private var _dresser = Set[Gear]()
  private var _sets = Set[String]()

  def addGear(item: Gear):Set[Gear] = {
    _sets = _sets + item.set
    _dresser = _dresser + item
    _dresser
  }

  def addGear(item: Set[Gear]):Set[Gear] = {
    _sets = _sets + item.head.set
    _dresser = _dresser ++ item
    _dresser
  }

  def removeGear(name: String):(Option[Gear], Set[Gear]) = {
    val gear = _dresser.find(e => e.name.equals(name))
    if (gear.isDefined) _dresser = _dresser - gear.get
    else logger.debug(s"\n***Gear $name not found in dresser***")
    (gear, _dresser)
  }

  def removeSet(name: String):Set[Gear] = {
    val gearSet = _dresser.filter(e => e.set.equals(name))
    _dresser = _dresser -- gearSet
    if (gearSet.isEmpty) logger.debug(s"\n***Gear set $name not found in dresser***")
    gearSet
  }

  def getGear(name: String):Option[Gear] = {
    val item = _dresser.find(e => e.name.equals(name))

    if (!item.isDefined) logger.debug(s"\n***Gear $name not found in dresser***")

    item
  }
}

object Dresser {

  def apply():Dresser = new Dresser()

  def getGearType(gear: Gear):Class[_] = {
    gear match {
      case _: HeadGear => HeadGear.getClass
      case _: TorsoGear => TorsoGear.getClass
      case _: LegGear => LegGear.getClass
      case _: FootGear => FootGear.getClass
      case _ => null
    }
  }

  def isSameType(gear: Gear, clazz: Class[_]) = {
    getGearType(gear).equals(clazz)
  }

}
