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

    //head: A, tail: List[A]
    def drop[A](list: List[A], n: Int): List[A] = {
        if (n <= 0) list
        else list match {
            case Nil => Nil
            case Cons(_, xs) => drop(xs, n-1)
        }
    }

    def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
        list match {
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => list
        }
    }

    //dropWhile2(xs) is returning a function, which is called with the argument f (currying). Grouping arguments like this is to assist with type inference. When a function contains multiple arg groups, type info flows from left to right. Since the first argument fixed the type param of A to ?, type annotation isn't necessary for f() when calling.
    //ie: dropWhile2(xs)(x => x < 4)
    def dropWhile2[A](list: List[A])(f: A => Boolean): List[A] =
        list match {
            case Cons(h, t) if f(h) => dropWhile2(t)(f)
            case _ => list
        }

        //given List(1,2,3,4), return List(1,2,3)
    def init[A](list: List[A]): List[A] = {
       list match {
            case Nil => Nil
            case Cons(_, Nil) => Nil
            case Cons(h,t) => Cons(h, init(t))
       }
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B) : B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int =
        foldRight(as, 0)((x, y) => y + 1)
    
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(h,t) => foldLeft(t, f(z,h))(f)
        }
    
    def sumFoldLeft(l: List[Int]) =
        foldLeft(l, 0)((acc, el) => acc + el)

    def reverse[A](l: List[A]): List[A] = 
        foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    
        
}       
