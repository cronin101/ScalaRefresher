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
        for (i <- nums if i % 2 == 0) yield i / 2
    }

    /** Recursion */
    def halveEvensRec(nums: List[Int]): List[Int] = {
        def recStep(acc: ListBuffer[Int], rest: List[Int]): List[Int] = {
            rest match {
                case List()  => acc.toList
                case x :: xs => if (x % 2 == 0) recStep(acc += x / 2, xs)
                                else            recStep(acc, xs)
            }
        }
        recStep(ListBuffer(), nums)
    }

    /** Higher-order Functions */
    def halveEvensHigher(nums: List[Int]): List[Int] = {
        nums.filter(x => x % 2 == 0).map(x => x / 2)
    }

    /** 2: Returning all numbers in the input list between two bounds (inclusive) */

    /** List Comprehension */
    def inRangeComp(nums: List[Int], lower: Int, upper: Int): List[Int] = {
        for (i <- nums if lower until upper contains i) yield i
    }

    /** Recursion */
    def inRangeRec(nums: List[Int], lower: Int, upper: Int): List[Int] = {
        def recStep(acc: ListBuffer[Int], rest: List[Int]): List[Int] = {
            rest match {
                case List()  => acc.toList
                case x :: xs => if (lower until upper contains x) recStep(acc += x, xs)
                                else                              recStep(acc, xs)
            }
        }
        recStep(ListBuffer(), nums)
    }

    /** Higher-order Functions */
    def inRangeHigher(nums: List[Int], lower: Int, upper: Int): List[Int] = {
        nums.filter(x => lower until upper contains x)
    }
    
        
    /** 3: Count the number of positive elements in the input list */
    
    /** List Comprehension */
    def countPositivesComp(nums: List[Int]): Int = {
        (for (i <- nums if i > 0) yield i).sum
    }
    
    /** Recursion */
    def countPositivesRec(nums: List[Int]): Int = {
        def recStep(acc: Int, rest: List[Int]): Int = {
            rest match {
                case List()  => acc
                case x :: xs => if (x > 0) recStep(acc + 1, xs)
                                else       recStep(acc, xs) 
            }
        }
        recStep(0, nums)
    }
    
    /** Higher-order Functions */
    def countPositivesHigher(nums: List[Int]): Int ={
        nums.filter(x => x > 0).map(_ => 1).sum
    }
    
    /** 4: Professor Pennypincher will not buy anything if he has to pay more than £199.00.
     *  But, as a member of the Generous Teachers Society, he gets a 10% discount on anything he buys.
     *  Write a function pennypincher that takes a list of prices and returns the total amount that
     *  Professor Pennypincher would have to pay,
     *  if he bought everything that was cheap enough for him. */
    var pincherLimit = 199.0
    
    /** List Comprehension */
    def pennyPincherComp(prices: List[Double]): Double = {
        (for (price <- prices if price * 0.9 <= pincherLimit) yield price * 0.9).sum
    }
    
    /** Recursion */
    def pennyPincherRec(prices: List[Double]): Double = {
        def recStep(acc: Double, rest: List[Double]): Double = {
            rest match {
                case List()  => acc
                case x :: xs => var price = x * 0.9
                                if (price <= pincherLimit) recStep(acc + price, xs)
                                else                         recStep(acc, xs)
            }
        }
        recStep(0.0, prices)
    }
    
    /** Higher-order Functions */
    def pennyPincherHigher(prices: List[Double]): Double = {
        prices.map(x => x * 0.9).filter(x => x <= pincherLimit).sum
    }
    
    /** 5: Write a function that returns the product of all digits in the input string.
     *  If there are no digits, the function should return the identity: 1 */
    
    /** List Comprehension */
    def multDigitsComp(string: List[Char]): Int = {
        (for (char <- string if char.isDigit) yield char.asDigit).foldLeft(1)(_*_)
    }
    
    /** Recursion */
    def multDigitsRec(string: List[Char]): Int = {
        def recStep(acc: Int, rest: List[Char]): Int = {
            rest match {
                case List()      => acc
                case char :: str => if (char.isDigit) recStep(acc * char.asDigit, str)
                                    else              recStep(acc, str)
            }
        }
        recStep(1, string)
    }
    
    /** Higher-order Functions */
    def multDigitsHigher(string: List[Char]): Int = {
        string.filter(_.isDigit).map(_.asDigit).foldLeft(1)(_*_)
    }
}