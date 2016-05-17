//trait => new data type; abstract interface
//sealed => all implementations of trait must be declared in same file

//3.1 3rd option, ret 3

//new collections => reuse old collection (don't copy) => data sharing
    //we can always ret immutable data w/o worrying about side effects

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0,_) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))


    def tail[A](list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_, xs) => xs
    }

    def setHead[A](head: A, list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_, list) => Cons(head, list)
    }
}       
