package lessons.designpatterns.creational

// Abstract Factory uses a 'factory interface' as parameter to create objects (mysql, pgsql)
// USES COMPOSITION

// Need to have in different package in order to run it
package appdesign.reational.abstractfactory {
  // FACTORY DRIVER
  trait SqlConnection {
    def getName: String
    def executeQuery(sql: String): Unit
  }

  class MySqlConnection extends SqlConnection {
    override def getName: String = "MySQL"

    override def executeQuery(sql: String): Unit = println(s"$getName => Executed: $sql")
  }

  class PgSqlConnection extends SqlConnection {
    override def getName: String = "PgSql"

    override def executeQuery(sql: String): Unit = println(s"$getName => Executed: $sql")
  }

  // ABSTRACT FACTORY PARAMETER - Composition
  trait DatabaseClientAbstractFactory {
    def connect(): SqlConnection
    def close(): Unit
  }

  class MySqlFactory extends DatabaseClientAbstractFactory {
    override def connect(): SqlConnection = {
      println("Connecting to MySql")
      new MySqlConnection
    }

    override def close(): Unit = {
      println("Closing MySql connection")
    }
  }

  class PgSqlFactory extends DatabaseClientAbstractFactory {
    override def connect(): SqlConnection = {
      println("Connecting to PgSql")
      new PgSqlConnection
    }

    override def close(): Unit = {
      println("Closing PgSql connection")
    }
  }

  // FACTORY - Creates different sql connections
  // You have decomposed MySqlClient/PgSqlClient with DatabaseClient
  class DatabaseClient(factoryConnection: DatabaseClientAbstractFactory) {
    private var _connection: SqlConnection = null

    // This is still Factory Method because Abstract Factory is about factory abstractions
    def connection: SqlConnection = {
      if (connection == null) _connection = factoryConnection.connect()
      _connection
    }

    def executeQuery(sql: String): Unit = {
      connection.executeQuery(sql)
    }
  }

  object AbstractFactory extends App {
    // DatabaseClient HAS A MySqlClient/PgSqlClient
    val mysqlClient = new DatabaseClient(new MySqlFactory)
    val pgsqlClient = new DatabaseClient(new PgSqlFactory)
  }

}
