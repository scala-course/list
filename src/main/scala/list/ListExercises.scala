package list

import scala.annotation.tailrec


object ListExercises {

  def removeDuplicates[Z](list: List[Z]): List[Z] = {
    var finalList = List[Z]()
    separateInDuplicates(list).foreach(a => {
      finalList = finalList ::: List(a.head)
    })
    finalList
  }

  def separateInDuplicates[Z](l: List[Z]): List[List[Z]] = {
    @tailrec
    def add(l: List[Z], lTemp: List[List[Z]], cont: Int): List[List[Z]] = {
      if (l.isEmpty) lTemp
      else if (l.tail.nonEmpty && l.head.equals(l.tail.head)) {
        add(l.tail, lTemp, cont + 1)
      } else {
        add(l.tail, lTemp ::: List(List.fill(cont + 1)(l.head)), 0)
      }
    }
    add(l, List[List[Z]](), 0)
  }

  def isPalindrome[Z](list: List[Z]): Boolean = {
    list.reverse.equals(list)
  }

  def duplicate[Z](times: Int, list: List[Z]): List[Z] = {
    var lAcum = List[Z]()
    list.foreach(a => {
      lAcum = lAcum ::: List.fill(times)(a)
    })
    lAcum
  }

  def reverse[Z](list: List[Z]): List[Z] = {
    var lAcum = List[Z]()
    list.foreach(a => {
      lAcum = List(a) ::: lAcum
    })
    lAcum
  }

  def reverseTailRec[Z](list: List[Z]):List[Z]= {
    @tailrec
    def add(list: List[Z], listResult:List[Z]):List[Z] = {
      if(list.isEmpty)  listResult
      else add(list.tail, List(list.head) ::: listResult)
    }
    add(list, List())
  }

  def sumOdd(list: List[Int]): Int = {
    list.filter(_ % 2 != 0).sum
  }
}
