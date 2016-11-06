def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)


val n = List(1,2,5,7,3,22,4,56,98,43)

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if(n == 0) xs
  else{
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if(x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
  }
}

msort(n)


def msort2[T](xs: List[T])(lt: (T,T) => Boolean): List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else{
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if(lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort2(fst)(lt), msort2(snd)(lt))
  }
}

msort2(n)((x: Int, y: Int) => x < y)

val fruits = List("apples", "oranges", "pineapples", "bananas")
msort2(fruits)((x: String, y: String) => x.compare(y) < 0)


import math.Ordering
def msort3[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else{
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if(ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort3(fst)(ord), msort3(snd)(ord))
  }
}

msort3(fruits)(Ordering.String)




def msort4[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else{
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if(ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort4(fst), msort4(snd))
  }
}


msort4(fruits)
