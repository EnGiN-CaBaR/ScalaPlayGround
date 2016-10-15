package week1

/**
  * Created by engin on 30.09.2016.
  */
object test {

  def balance(chars: List[Char]): Boolean = {
    def check(i: Int, c: Char, chars: List[Char]): Int =
      if (chars.isEmpty) i
      else if (c == '(') check(i + 1, chars.head, chars.tail)
      else if (c == ')' && i > 0) check(i - 1, chars.head, chars.tail)
      else check(i, chars.head, chars.tail)
    if (check(0, chars.head, chars.tail) != 0) false
    else true
  }


  def main(args: Array[String]) {

    println(balance("())(()".toList))
  }


  "(if (zero? x) max (/ 1 x))"
  "I told him (that it’s not (yet) done). (But he wasn’t listening)"
  ":-)"
  "())("

}
