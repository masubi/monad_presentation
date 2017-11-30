package proj

import scalaz.{Functor}

// sbt console:  "runMain proj.MyFunctor"
object MyFunctor extends FunctorHelper {

  def main(args: Array[String]) {

    //
    //  Functor
    //
    printTitle("Example Functor: given A=>B, find C[A]=>C[B]")

    println("suppose:     A==Int, B==String")

    val demoFunctor = new Functor[C]{
      def map[A,B](ca:C[A])(f:A=>B):C[B] = C(f(ca.first),f(ca.second))
    }

    // C[A]
    val ca: C[Int] = new C[Int](1,2)
    println("suppose:     C[A] == "+ca)

    // A=>B == (a: Int)=>a.toString()+"_transformed"
    val fn: Int=>String = AtoB
    println("suppose:     (A=>B) == ( (a: Int)=>transformed+a.toString() )")

    //  C[B]
    println("then:        C[A]=>C[B] == (C[B]=C[A].map(A=>B))")
    println("aka:         C[String] = C[Int].map(Int=>String)")
    val cb: C[String] = demoFunctor.map[Int, String](ca)(fn)
    println("results in:  C[B] == C[String] == " + cb+"\n")
  }
}
