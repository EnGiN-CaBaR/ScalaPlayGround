package week4

/**
  * Created by engin on 27.10.2016.
  */
object SortingExample {

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
}
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
  }

  def main(args: Array[String]) {
    val a = List(7,3,9,2)
    println(isort(a))
  }

}
