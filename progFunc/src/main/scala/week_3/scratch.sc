import week_3.{Rational}

object scratch{
  new Rational(1, 4)


  def error(msg: String) = throw new Error(msg)

  //    error("test")

  val x = null
  val y: String = x

  //    val z: Int = null error: type mismatch

  if(true) 1 else false
}