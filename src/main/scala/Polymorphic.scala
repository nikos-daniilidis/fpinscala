/**
  * Created by nikos on 8/26/16.
  */
object  Polymorphic {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
    if (as.length == 1) true
    else if (ordered(as(0), as(1))) isSorted(as.tail, ordered)
    else false

  def curry[A, B, C](f:  (A, B) => C): A => (B  => C) = a => (b => f(a, b))

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
