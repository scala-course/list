package list

import scala.annotation.tailrec


object ListExercises {

  def removeDuplicates(l: List[Symbol]): List[Symbol] = {
    var finalList = List[Symbol]()
    separateInDuplicates(l).foreach(a => {
      finalList = finalList ::: List(a.head)
    })
    finalList
  }

  def separateInDuplicates(l: List[Symbol]): List[List[Symbol]] = {
    @tailrec
    def add(l: List[Symbol], lTemp: List[List[Symbol]], cont: Int): List[List[Symbol]] = {
      if (l.isEmpty) lTemp
      else if (l.tail.nonEmpty && l.head.equals(l.tail.head)) {
        add(l.tail, lTemp, cont + 1)
      } else {
        add(l.tail, lTemp ::: List(List.fill(cont + 1)(l.head)), 0)
      }
    }
    add(l, List[List[Symbol]](), 0)
  }

  def isPalindrome(l: List[Symbol]): Boolean = {
    l.reverse.equals(l)
  }

  def duplicate(times: Int, list: List[Symbol]): List[Symbol] = {
    var lAcum = List[Symbol]()
    list.foreach(a => {
      lAcum = lAcum ::: List.fill(times)(a)
    })
    lAcum
  }

  def reverse(list: List[Symbol]): List[Symbol] = {
    var lAcum = List[Symbol]()
    list.foreach(a => {
      lAcum = List(a) ::: lAcum
    })
    lAcum
  }

  def sumOdd(list: List[Int]): Int = {
    list.filter(_ % 2 != 0).sum
  }
}
