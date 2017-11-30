package proj

trait FunctorHelper {
  def printTitle(msg: String)={
    println("\n---------------------------------------------------------------")
    println(msg)
    println("---------------------------------------------------------------\n")
  }

  // Container
  case class C[A](first:A,second:A)
  case class FnContainer[A, B](fn: A=>B)

  // A=>B defined some what arbitrarily below
  val AtoB: Int=>String = (a: Int)=>a.toString()+"_transformed"

}
