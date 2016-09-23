/**
  * Created by nikos on 9/8/16.
  */
type St[S, +A] = S => (A, S)

trait State[S, +A] {
  def nextItem: (A, S)
}

case class BaseState[S, +A](s: S, run: St[S, A]) extends State{
  def nextItem: (A, S) = run(s)

  def unit[A](a: A): (A, S) = (a, s)

  def map[A, B](f: A => B): St[S, B] = {
    s0 => {
      val (a: A, s1: S) = run(s0)
      (f(a), s1)
    }
  }

  // TODO: Which state does it make most sense for map2 to pass back?
  def map2[A, B, C](sa: St[S, A], sb: St[S, B])(f: (A, B) => C): St[S,C] = {
    s0 => {
      val (a: A, s1: S) = sa(s0)
      val (b: B, s2: S) = sa(s0)
      (f(a, b), s1)
    }
  }

  def sequence[A](fs: List[St[S, A]]): St[S, List[A]] = {
    s0 => {
      val (l: List[A], s1: S) = fs.map(f => f(s0))
      (l, s1)
    }
  }

  def flatMap[A, B](f: St[S, A])(g: A => St[S, B]): St[S, B] = {
    s0 => {
      val (a: A, s1: S) = f(s0)
      val (b: B, s2: S) = g(a)(s0)
      (b, s1)
      }
    }

}

object State {
}