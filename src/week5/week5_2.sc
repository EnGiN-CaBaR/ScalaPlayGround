
def squareList(xs: List[Int]): List[Int] = {
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList(ys)
  }
}


def squareListShorter(xs: List[Int]): List[Int] =
  xs map (x => x*x)



def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))


val l = List(1,2,3,4,5,6)
def sum(xs: List[Int]) = xs reduceLeft((x, y) => x + y)
sum(l)

def sumShorter(xs: List[Int]) = xs reduceLeft(_ + _)


def sumFoldLeft(xs: List[Int]) = (xs foldLeft 0)(_ + _)
sumFoldLeft(l)


def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)