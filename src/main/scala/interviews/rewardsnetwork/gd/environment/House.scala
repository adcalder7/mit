package interviews.rewardsnetwork.gd.environment

case class House private() {
  private var _user:User = null
  private var _dresser:Dresser = null

  // Getter
  def dresser = _dresser
  def user = _user

  def addUser(user:User):Unit = {
    if (_user == null) _user = user
  }

  def addDresser(dresser: Dresser):Unit = _dresser = dresser
}

object House {
  def apply(user:User, dresser: Dresser):House = {
    val house = House()
    house.addUser(user)
    house.addDresser(dresser)
    house
  }
}