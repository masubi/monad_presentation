package proj

import scalaz.Functor

// sbt console:  "runMain proj.Monad"
object Monad extends FunctorHelper {

  def main(args: Array[String]) {
    //
    //  Example Monad 1
    //
    // Note:  applicatives are functors
    printTitle("Example Monad 1: given A=>[B], find C[A]=>C[B]")

    val demoMonad = new Functor[C]{

      // Functor
      def map[A,B](ca:C[A])(f:A=>B):C[B] = C(f(ca.first),f(ca.second))

      // Applicative
      def apply[A, B](ca: C[A])(fnC: FnContainer[A, B]): C[B] = {
        new C(fnC.fn(ca.first), fnC.fn(ca.second))  // define as we want as long as returns C[B]
      }

      // Monad Example 1
      def flatMap[A,B](ca:C[A])(fn: A=>C[B]): C[B] = {
        flatten(map(ca)(fn))
      }
      def flatten[B](ccb: C[C[B]]): C[B]  = C[B](ccb.first.second, ccb.second.second) // can define as desired
    }

    // C[C[A]]
    val cca: C[C[Int]] = C(C(1,2), C(3,4))
    println("suppose:  C[A] == C[C[Int]] == "+ cca)

    // A=>C[B] = C[Int]=>C[C[String]]
    // defined somewhat arbitrarily
    def fn(a: C[Int]):C[C[String]] = new C(
      new C(AtoB(a.first), AtoB(a.second)),
      new C(AtoB(a.first)+"_augmented", AtoB(a.second)+"_augmented"))
    println("suppose:  A=C[Int], C[B]=C[C[String]], then A=>C[B] == C[Int]=>C[C[String]]")

    // C[B]
    val cb_1 = demoMonad.flatMap[C[Int], C[String]](cca)(fn) 
    println("results in:  C[B] == "+cb_1.toString())


    //
    //  Example Monad 2
    //
    printTitle("Example Monad 2")

    val demoMonad2 = new Functor[C]{

      // Functor
      def map[A,B](ca:C[A])(f:A=>B):C[B] = C(f(ca.first),f(ca.second))

      // Applicative
      def apply[A, B](ca: C[A])(fnC: FnContainer[A, B]): C[B] = {
        new C(fnC.fn(ca.first), fnC.fn(ca.second))  // define as we want as long as returns C[B]
      }

      // Monad Example 2
      def flatMap2[A,B](ca:C[A])(fn: A=>C[B]): C[B] = {
        val mapRes = map(ca)(fn)
        println("map result: "+ mapRes)
        val flattenRes = flatten2(map(ca)(fn))
        println("flatten result: "+flattenRes)
        flattenRes
      }
      def flatten2[B](cb: C[C[B]]): C[B]  = C(cb.first.first, cb.second.first)
    }
    println("recall:  given A=>[B], find C[A]=>C[B]")

    // C[C[A]]
    // reuse cca == C(C(1,2), C(3,4))
    println("suppose:  C[A] == C[C[Int]] == "+ cca) //

    // A=>C[B] = C[Int]=>C[String]
    def fn2(a: C[Int]):C[String] = new C(AtoB(a.first), AtoB(a.second)) //defined somewhat arbitrarily
    println("suppose:  A==C[Int] && C[B]==C[String], then A=>C[B] == C[Int]=>C[String]")
    println("define: C[Int]=>C[String] as (a: C[Int]) => { new C(AtoB(a.first), AtoB(a.second)) }")

    // C[B]
    println("C[B] = C[A].flatMap(fn2) or C[String] = C[C[Int]].flatMap(fn2)")
    val cb_2 = demoMonad2.flatMap2[C[Int], String](cca)(fn2)
    println("flatmap result:  C[B] == "+cb_2.toString())

  }

}