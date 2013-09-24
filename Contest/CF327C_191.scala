object CF327C_191 extends App {
  val s = Console.readLine
  val n : Long = s.size.toLong
  val k : Long = Console.readLine.toLong
  val md = 1000000007L

  def p2(y : Long, x : Long) : Long = {
    if (x == 0) 1 else {
      val t = p2(y, x/2)
      if (x % 2 == 0) (t * t) % md else (((y * t) % md) * t) % md
    }
  }

  def inv(x : Long) : Long = {
    p2(x, md - 2)
  }

  val res = (0 until n.toInt).map{
    x => if (s(x) == '0' || s(x) == '5') p2(2, x) else 0
  }.reduce((x, y) => (x + y) % md)

  val outer = (p2(2, k * n) - 1 + md) % md * inv((p2(2, n) - 1 + md)% md) % md

  println (res * outer % md)
}
