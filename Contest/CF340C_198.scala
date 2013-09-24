object CF340C_198 extends App{
  def gcd (x : Long, y : Long) : Long = {
    if (x % y == 0) y else gcd (y, x % y)
  }
  val n = Console.readLine.toInt
  val a = Console.readLine.split(' ').map(_.toLong).sorted.zip(1 to n).map{case (x, i) =>
    (4 * i - 2 * n - 1) * x
  }.reduce(_ + _)
  val d = gcd(a, n)
  printf("%d %d\n", a/d, n/d)
}
