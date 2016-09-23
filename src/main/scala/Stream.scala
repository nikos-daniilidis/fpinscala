package fpinscala.lazyval
/**
  * Created by nikos on 9/2/16.
  */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def listAcc(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => listAcc(t(), acc:::(h()::Nil))
    }
    listAcc(this, Nil)
  }

  def take(n: Int): Stream[A] = {
    def takeAcc(s: Stream[A], acc: Stream[A], m: Int): Stream[A] =
      if (m==0) acc else s match {
        case Empty => acc
        case Cons(h, t) => takeAcc(t(), Cons(() => h(), () => acc), m-1)
    }
    takeAcc(this, Empty, n)
  }

  def drop(n: Int): Stream[A] = {
    def dropRem(s: Stream[A], m: Int): Stream[A] =
      if (m==0) s else s match {
        case Empty => s
        case Cons(h, t) => dropRem(t(), m-1)
      }
    dropRem(this, n)
  }

  def takeWhile0(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if (p(h())) Cons(() => h(), () => t().takeWhile0(p))
      else t().takeWhile0(p)
    } // TODO: Consider adding lazy val for h and t
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists0(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
  // also !exists(x: A => !p(x) )

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty)((a, b) => if (p(a)) b else Empty)

  //TODO: headOption using foldRight

  def map[B](f: A => B): Stream[B] = {
    val empty: Stream[B] = Empty
    foldRight(empty)((a, b) => Cons(() => f(a), () => b))}

  def filter(p: A => Boolean) = {
    val empty: Stream[A] = Empty
    foldRight(empty)((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else b)
  }

  // TODO: fix typing deadlock when a: A
  def append[B](a: => B): Stream[A] = {
    val empty: Stream[A] = Empty
    foldRight(empty)((x, y) => Cons(() => x, () => y))
  }

  // TODO: check when online
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    val empty: Stream[B] = Empty
    //foldRight(empty)((x, y) => foldRight(f(x))((u, v) =>  Cons(() => u, () => v)))
    foldRight(empty)((x, y) => foldRight(y)((u, v) => f(u)))
  }

  def constant[A](a: A): Stream[A] = Cons(() => a, () => this.constant(a))

  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some(a: A, s: S) => Cons(() => a, () => unfold(s)(f))
    case _ => Empty
  }

  // TODO: Ex. 5.12 and on
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head: A, apply(as.tail: _*))
  }

  def fibs: Stream[Int] = {
    def fibStream(m: Int, n: Int): Stream[Int] = Cons(() => m+n, () => fibStream(m+n, m))
    fibStream(1, 0)
  }
}