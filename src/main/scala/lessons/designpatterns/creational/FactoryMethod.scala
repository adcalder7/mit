package lessons.designpatterns.creational

// Factory Method is just a method that gets overriden to change the return type (mysql, pgsql)
// USES INHERITANCE

trait SqlConnection {
  def getName:String
  def executeQuery(sql:String):Unit
}

class MySqlConnection extends SqlConnection {
  override def getName: String = "MySQL"
  override def executeQuery(sql: String): Unit = println(s"$getName => Executed: $sql")
}

class PgSqlConnection extends SqlConnection {
  override def getName: String = "PgSql"
  override def executeQuery(sql: String): Unit = println(s"$getName => Executed: $sql")
}

// Client Factory
trait DatabaseClient {
  private var _connection:SqlConnection = null

  // This is the factory method because it returns mysql or pgsql etc
  def connection:SqlConnection = {
    if (connection == null) _connection = connect()
    _connection
  }

  def executeQuery(sql:String):Unit = {
    connection.executeQuery(sql)
  }

  // Connect is a factory method
  protected def connect():SqlConnection

  // NOTE: If we have a lot of factory methods then we need to switch to abstract factory

  def close():Unit
}

class MySqlClient(ip:String, port:Int) extends DatabaseClient {
  override def connect(): SqlConnection = {
    println(s"Connecting to MySql using $ip and $port")
    new MySqlConnection
  }
  override def close(): Unit = {
    println("Closing MySql connection")
  }
}

class PgSqlClient(ip:String) extends DatabaseClient {
  override def connect(): SqlConnection = {
    println(s"Connecting to PgSql using $ip")
    new PgSqlConnection
  }
  override def close(): Unit = {
    println("Closing PgSql connection")
  }
}

object FactoryMethod extends App {
  // DatabaseClient IS A MySqlClient/PgSqlClient
  val mysqlClient:DatabaseClient = new MySqlClient("1.1.1.1", 123)
  val pgsqlClient:DatabaseClient = new PgSqlClient("1.1.1.1")
}
