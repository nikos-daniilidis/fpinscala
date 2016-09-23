package fpinscala.datastructures
/**
  * Created by nikos.daniilidis on 8/3/16.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(as: List[Int]): Int =  as match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(as: List[Int]): Int = as match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =  // A* makes apply variadic: accepts zero or more A types
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))  // this is how you pass Seq to a function expecting variadic

  def tail[A](as: List[A]) = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](h: A, as: List[A]) = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n==0) as
    else drop(tail(as), n-1)

  // by currying f, no need for referencing A on calling dropWhile
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(x, xs) => if (f(x)) xs else Cons(x, dropWhile(xs)(f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def foldAcc(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => foldAcc(xs, f(acc, x))
    }
    foldAcc(as, z)
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    lazy val reversed = reverse2(as)
    foldLeft(reversed, z)((x,y) => f(y,x))
  }

  def sum2(as: List[Int]): Int = foldRight(as, 0)(_ + _)  // aka  (x, y) => x + y

  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product2(as: List[Int]): Int = foldRight(as, 1)(_ * _)

  def product3(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((bs,l) => l + 1)

  def reverse[A](as: List[A]): List[A] = as match {
    case Cons(x, Nil) => as
    case Cons(x, xs) => append(reverse(xs), Cons(x, Nil))
  }

  def reverse2[A](as: List[A]): List[A] = foldLeft(as: List[A], Nil: List[A])((a,b) => Cons(b,a))

  def reverse3[A](as: List[A]): List[A] = {
    def revAcc[A](bs: List[A], acc: List[A]): List[A] = bs match {
      case Nil => acc
      case Cons(x, xs) => revAcc(xs, append(Cons(x, Nil), acc))
    }
    revAcc(as, Nil)
  }

  def append2[A](as: List[A], bs: List[A]): List[A] = foldRight2(as, bs)(Cons(_, _))

  def concatenate[A](as: List[List[A]]): List[A] = foldRight2(as, Nil: List[A])((a, b) => append2(a, b))

  def addOne(as: List[Int]): List[Int] = foldRight2(as, Nil: List[Int])((a, b) => Cons(a+1, b))

  def toString(as: List[Double]): List[String] = foldRight2(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight2(as, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs, f)) else filter(xs, f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def flatAcc(as: List[A], acc: List[B]): List[B] = as match {
      case Nil => Nil
      case Cons(x, xs) => flatAcc(xs, append2(f(x), acc))
    }
    flatAcc(as, Nil)
  }

  def filter2[A](as: List[A], f: A => Boolean): List[A] = {
    def filtList(x: A): List[A] = if (f(x)) Cons(x, Nil) else Nil
    flatMap(as)(filtList)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    def zipAcc(as: List[A], bs: List[B], acc: List[C]): List[C] = as match {
      case Nil => bs match {
        case Nil => acc
        case Cons(y, ys) => Nil
      }
      case Cons(x, xs) => bs match {
        case Nil => Nil
        case Cons(y, ys) => zipAcc(xs, ys, append2(Cons(f(x, y), Nil), acc))
      }
    }
    zipAcc(as, bs, Nil)
  }

  // TODO: List hasSubsequence
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}
