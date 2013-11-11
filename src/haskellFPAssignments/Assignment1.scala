package haskellFPAssignments
import scala.collection.mutable.ListBuffer

object Assignment1 {
	def main(args: Array[String]) {
		var halveInput = List(0,2,1,7,8,56,17,18)
		println(halveEvensComp(halveInput))
		println(halveEvensRec(halveInput))
		println(halveEvensHigher(halveInput))
	}
	
	/** Returning a list of half of all the even input elements */
	
	/** List Comprehension */
	def halveEvensComp(nums: List[Int]): List[Int] = {
		for (i <- nums if i % 2 == 0)
			yield i / 2
	}
	
	/** Recursion */
	def halveEvensRec(nums: List[Int]): List[Int] = {
		def recStep(acc: ListBuffer[Int], left: List[Int]): List[Int] = {
			left match {
			  case List()      => acc.toList 
			  case x :: xs     => if (x % 2 == 0)
				  					recStep(acc += x/2, xs)
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
}