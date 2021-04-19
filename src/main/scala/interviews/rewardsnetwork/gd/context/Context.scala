package interviews.rewardsnetwork.gd.context

import interviews.rewardsnetwork.gd.environment._

case class Context(house: House, day: Day)

object Context {

  // Initialize default context
  def apply(temperature:String): Context = Context(getDefaultHouse(), Day(temperature))

  private def getDefaultHouse():House = {
    val user = new User()
    val dresser = getDefaultDresser()

    // Dress user
    val pjSet = dresser.removeSet("PJs")
    pjSet.foreach(user.addGear)

    House(user, dresser)
  }

  private def getDefaultDresser():Dresser = {
    val dresser = Dresser()

    // Single items
    dresser.addGear(HeadGear("sunglasses"))
    dresser.addGear(HeadGear("hat"))
    dresser.addGear(TorsoGear("shirt"))
    dresser.addGear(TorsoGear("jacket"))
    dresser.addGear(LegGear("shorts"))
    dresser.addGear(LegGear("pants"))
    dresser.addGear(FootGear("sandals"))
    dresser.addGear(FootGear("boots"))
    dresser.addGear(FootGear("socks"))
    // Set
    dresser.addGear(HeadGear("pj hat", "PJs"))
    dresser.addGear(TorsoGear("pj shirt", "PJs"))
    dresser.addGear(LegGear("pj pants", "PJs"))
    dresser.addGear(FootGear("pj sandals", "PJs"))

    dresser
  }

}
