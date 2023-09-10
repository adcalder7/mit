package lessons.designpatterns.structural

// Hides complexity of many classes/libraries under one interface
// Often used as a driver

class CarEngine
class CarWheel
class CarSeats

class CarFacade {
  private val engine = new CarEngine()
  private val wheel = new CarWheel()
  private val seats = new CarSeats()

  def seat():Unit = { engine }
  def drive():Unit = { wheel }
  def stop():Unit = { seats }
}

object FacadeTest extends App {
  // Client is only using the facade and is not worried about CarFacade
  val car = new CarFacade()
  car.seat()
  car.drive()
  car.stop()
}
