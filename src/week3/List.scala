package week3

import java.util.NoSuchElementException

/**
  * Created by engin on 15.10.2016.
  */
trait List[T] {

  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends List[T]{
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List{
  //List(1, 2) List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil

}

object nth{
  def nth[T](n: Int, xs: List[T]): T =
    if(xs.isEmpty) throw new IndexOutOfBoundsException
    else if(n == 0) xs.head
    else nth(n - 1, xs.tail)

  def main(args: Array[String]) {
    val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println(nth(2, list))
    println(nth(-1, list))
  }
}