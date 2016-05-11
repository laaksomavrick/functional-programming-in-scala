object abs {

    def abs(n: Int): Int =
        if (n < 0) -n
        else n

    def factorial(n: Int) : Int = {

        def go(n: Int, acc: Int): Int =
            if (n <= 0) acc
            else go(n-1, n*acc)

        go(n, 1)

    }

    def fib(n: Int) : Int = 
            if (n <= 1) n 
            else fib(n - 1) + fib(n - 2)

    def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
        b => f(a, b)

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        a => (b => f(a, b))
    
    def compose[A, B, C](f: B => C, g: A => B): A => C = 
        a => f(g(a))

     def formatResult(name: String, n: Int, f: Int => Int) = {

        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))

    }

    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= as.length) -1
            else if (p(as(n))) n
            else loop(n + 1)

        loop(0)

    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean =
            if (n >= as.length - 1) true
            else if (ordered(as(n), as(n+1))) loop(n + 1)
            else false

        loop(0)
    }
    

    def main(args: Array[String]) : Unit =
        println("Hello, world!")
        println(formatResult("abs", -42, abs))
        println(formatResult("fib", 6, fib))
        println(formatResult("fac", 5, factorial))

}
