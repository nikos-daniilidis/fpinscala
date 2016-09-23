/**
  * Created by nikos on 9/2/16.
  */
type Rand[+A] = RNG => (A, RNG)

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2), rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1)  = rng.nextInt
    if (i1 < 0 && i1 > Int.MinValue) (-i1, rng1)
    else if  (i1 >= 0) (i1, rng1)
    else (0, rng1)
  }

  // TODO: exlude 1.0 from the results?
  def double0(rng: RNG): (Double, RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)
    ((i1/Int.MaxValue).toDouble,rng1)
  }

  def intDouble0(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (d, rng2) = double0(rng1)
    ((i1, d), rng2)
  }

  // rewrite nextInt using Rand
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // map the output of a state, without changing the state
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[(C)] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng)
      (f(a, b), rng)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val l = fs.map(f => f(rng)._1)
      (l, rng)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))  //((a, b) => (a, b))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val a = f(rng)._1
      val b = g(a)(rng)._1
      (b, rng)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(nonNegativeInt)(i => (i/Int.MaxValue).toDouble)

  def intDouble: Rand[(Int, Double)] = both(int, double)

  def nonNegativeLessThan(m: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      rng => {
        val mod = i % m
        if (i + (m - 1) - mod >= 0)
          (mod, rng)
        else nonNegativeLessThan(m)(rng)
      }
    })
}