/**
  * Created by nikos on 9/2/16.
  */
object Strict {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  def maybeTwice(cond: Boolean, onTrue: => Int, onFalse: => Int): Int = {
    if (cond) {
      lazy val j = onTrue  // without lazy onTrue will be evaluated twicw
      j + j
    }
    else 0
  }
}
