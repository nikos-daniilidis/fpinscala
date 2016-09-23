package fpinscala.exceptions
/**
  * Created by nikos on 8/31/16.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(as: Seq[Double]): Option[Double] = {
    if (as.isEmpty) None
    else Some(as.sum/as.length)
    }

  // TODO: this looks dumb. Why check for empty in all these places?
  def variance(as: Seq[Double]): Option[Double] = {
    (if (as.isEmpty) None else Some(as)).
      flatMap(xs => {if (xs.isEmpty) None
      else Some(xs.map(x => x-xs.sum/xs.length))}).
      flatMap(xs => mean(xs.map(math.pow(_, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case None => None
    case Some(aa) => b match {
      case None => None
      case Some(bb) => Some(f(aa, bb))
    }
  }

  // TODO: omg this is so ugly!
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.exists(x => (x==None))) None
    else Some(
      a.map(x => x match {
        case Some(xx) => xx})
    )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def takeOne(as: List[A], acc: List[B]): Option[List[B]] = as match {
      case Nil => Some(acc)
      case x::xs => f(x) match {
        case None => None
        case Some(v) => takeOne(xs, acc:::(v::Nil))
      }
    }
    takeOne(a, Nil)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }
}