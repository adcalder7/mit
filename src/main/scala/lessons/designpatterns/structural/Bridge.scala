package lessons.designpatterns.structural

// Only happens during application design
// If a class contain concerns that can grow based on implementation then
// extract that into its own interface and implementations (make a bridge)

// Perfect example of using scala dependency: self:Hasher =>

// Hash and Password are growing together!!!
// Class explosion!
class SimplePasswordMD5
class SimplePasswordSHA1
class SimplePasswordSHA256

// FIX

trait Hash {
  def hash(str:String):String
}

trait MD5Hasher extends Hash {
  override def hash(str:String):String = ""
}

trait SHA1Hasher extends Hash {
  override def hash(str:String): String = ""
}

trait SHA256Hasher extends Hash {
  override def hash(str:String): String = ""
}

// Hash and Password are now decoupled
class SimplePassword {
  self:Hash =>
  def convert(pass:String):String = hash(pass)
}

object Bridge extends App {
  val md5Password = new SimplePassword with MD5Hasher
  val sha1Password = new SimplePassword with SHA1Hasher
  val sha256Password = new SimplePassword with SHA256Hasher
}
