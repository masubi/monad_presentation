package proj

import scalaz.Functor

// sbt console:  "runMain proj.Applicative"
object Applicative extends FunctorHelper {

  def main(args: Array[String]) {
    //
    //  Applicative
    //
    // Note:  applicatives are functors
    printTitle("Example Applicative: given C[A=>B], find C[A]=>C[B]")

    println("suppose:      A==Int, B==String")
    case class FnC[A, B](fn: A => B)  //FunctionContainer

    val demoApplicative = new Functor[C] {
      def map[A, B](ca: C[A])(f: A => B): C[B] = C(f(ca.first), f(ca.second))

      // C[A]=>C[B]
      def apply[A, B](ca: C[A])(fnC: FnC[A, B]): C[B] = {
        new C(fnC.fn(ca.first), fnC.fn(ca.second)) // can customize long as returns C[B]
      }
    }

    // C[A]
    val ca = new C(1, 2)
    println("suppose:      C[A] == " + ca)

    // C[A=>B]:  for A=>B reuse AtoB i.e. (a: A)=>"transformed_"+(a).toString()
    val fnContainer = new FnC[Int, String](AtoB)
    println("suppose:      C[A=>B] == C((a: Int)=>'transformed_'+a.toString())")

    // C[B]
    println("then:         (C[A]=>C[B]) == ( C[B]=C[A].apply(C[A=>B]) )")
    println("aka:          C[String]=C[Int].apply(C[Int=String])")
    val cb = demoApplicative.apply[Int, String](ca)(fnContainer)
    println("results in:   C[B] == " + cb.toString() + "\n")
  }

}