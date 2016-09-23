/**
  * Created by nikos on 9/2/16.
  */
object Recursion {
  def abs(n: Int): Int = if (n < 0) -n
  else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def fibonacci(n: Int) = {
    @annotation.tailrec
    def fib_accum(a: Int, b: Int, c: Int, acc: Int): Int =
      if (c==n) acc
      else fib_accum(b, acc, c+1, acc+b)
    fib_accum(0, 1, 0, 0)
  }

  def main(args: Array[String]): Unit = println(formatAbs(-42))
}
