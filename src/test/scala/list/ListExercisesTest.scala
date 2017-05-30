package list

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by fnovoa10 on 05/24/17.
  */
class ListExercisesTest extends FlatSpec with Matchers {

  import ListExercises._

  it should "When pass list with list must be return list with removed consecutive duplicates" in {
    val result = removeDuplicates(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    result.length should be(6)

    val result2 = removeDuplicates(List('b, 'b, 'w, 'w, 'w, 'w, 'w, 'w, 'w, 'w, 'w))
    result2.length should be(2)
  }

  it should "Be true when word is palidrome" in {
    isPalindrome(List('a, 'b, 'a)) should be (true)
    isPalindrome(List('s, 'o, 'b, 'r, 'e, 'v, 'e, 'r, 'b, 'o, 's)) should be (true)
  }


  it should "Be false when word is  not palidrome" in {
    isPalindrome(List('a, 'b, 'a, 's)) should be (false)
    isPalindrome(List('f, 'a, 'b, 'i, 'a, 'n)) should be (false)
  }

  it should "Duplicate any array" in {
    duplicate(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    duplicate(1, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'b, 'c, 'c, 'd))
    duplicate(2, List('a)) should be (List('a, 'a))
  }

  it should "Get reverse array without scala reverse function" in {
    reverse(List('a, 'b, 'c, 'd)) should be (List('d, 'c, 'b, 'a))
    reverse(List('f, 'a, 'b, 'i, 'a, 'n)) should be (List('n, 'a, 'i, 'b, 'a, 'f))
  }

  it should "Get reverse array without scala reverse function(tail rec)" in {
    reverseTailRec(List('a, 'b, 'c, 'd)) should be (List('d, 'c, 'b, 'a))
    reverseTailRec(List('f, 'a, 'b, 'i, 'a, 'n)) should be (List('n, 'a, 'i, 'b, 'a, 'f))
  }


  it should "Sum all odd numbers and return result" in {
    sumOdd(List(1, 2, 3, 4)) should be(4)
    sumOdd(List(1, 1, 1, 1, 1, 1, 1)) should be(7)
    sumOdd(List(2, 4, 6, 8)) should be(0)
  }

  it should "Pack consecutive duplicates of list elements into sublists." in {
    separateInDuplicates(List(1, 1, 1, 2, 2, 3, 1, 5, 5, 3, 3)) should be (List(List(1,1,1), List(2,2), List(3), List(1), List(5,5), List(3,3)))
    separateInDuplicates(List(1,2, 3, 3, 3, 3)) should be (List(List(1), List(2), List(3,3,3,3)))
    separateInDuplicates(List('a, 'a, 'b, 'b, 'a, 'a)) should be (List(List('a, 'a), List('b, 'b), List('a, 'a)))
    separateInDuplicates(List('a, 'b, 'c, 'c, 'c, 'c)) should be (List(List('a), List('b), List('c, 'c, 'c, 'c)))
  }
}
