package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 1 => t()
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  def takeWhile_1(p: A => Boolean): Stream[A] = foldRight(Empty)((a,b) => if(p(a)) Empty else Empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) => if(p(head())) tail().forAll(p) else false
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h),t))

  def append[B>:A](xs: => Stream[B]) = foldRight(xs)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def filter(f: A => Boolean) = foldRight(empty[A])((h,t) => if(f(h)) cons(h,t) else t)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = (this,s) match {
    case (Empty, _ ) => true
    case (_, Empty)  => false
    case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => t1().startsWith(t2())
    case _ => false
  }
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a:A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, Stream.from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match{
    case None => Empty
    case Some((h,s)) => cons(h,unfold(s)(f))
  }
}