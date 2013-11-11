package haskellFPAssignments
import scala.collection.mutable.ListBuffer

object Assignment1 {
  def main(args: Array[String]) {
    var halveInput = List(0, 2, 1, 7, 8, 56, 17, 18)
    println(halveEvensComp(halveInput))
    println(halveEvensRec(halveInput))
    println(halveEvensHigher(halveInput))
  }

  /** 1: Returning a list of half of all the even input elements */

  /** List Comprehension */
  def halveEvensComp(nums: List[Int]): List[Int] = {
    for (i <- nums if i % 2 == 0)
      yield i / 2
  }

  /** Recursion */
  def halveEvensRec(nums: List[Int]): List[Int] = {
    def recStep(acc: ListBuffer[Int], left: List[Int]): List[Int] = {
      left match {
        case List()  => acc.toList
        case x :: xs => if (x % 2 == 0)
                            recStep(acc += x / 2, xs)
                        else
                            recStep(acc, xs)
      }
    }
    recStep(ListBuffer(), nums)
  }

  /** Higher-order functions */
  def halveEvensHigher(nums: List[Int]): List[Int] = {
    nums.filter(x => x % 2 == 0).map(x => x / 2)
  }
  
  /** 2: Returning all numbers in the input list between two bounds (inclusive) */
  
  /** List Comprehension */
  def inRangeComp(nums: List[Int], lower: Int, upper: Int): List[Int] = {
    for (i <- nums if lower until upper contains i)
      yield i
  }
  
  /** Recursion */
  def inRangeRec(nums: List[Int], lower: Int, upper: Int): List[Int] = {
    def recStep(acc: ListBuffer[Int], left: List[Int]): List[Int] = {
      left match {
        case List()  => acc.toList
        case x :: xs => if (lower until upper contains x)
                            recStep(acc += x, xs)
                        else
                            recStep(acc, xs)
      }
    }
    recStep(ListBuffer(), nums)
  }
  
  /** Higher-order functions */
  def inRangeHigher(nums: List[Int], lower: Int, upper: Int): List[Int] = {
    nums.filter(x => lower until upper contains x)
  }
}