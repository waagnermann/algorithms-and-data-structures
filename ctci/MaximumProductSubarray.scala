import scala.math.max

/*Given an integer array nums, find the contiguous subarray within an array
 (containing at least one number) which has the largest product.*/

/*This solution contains exactly N iterations, where N is the size of array*/

object MaximumProductSubarray {

	def maxProduct(nums: Array[Int]): Int = {
        val volume = nums.length
        var maximumProduct = Int.MinValue
        var runningProduct = 1
        var divider = 1

        var i = 0
        while (i < volume) {
        	val currentNumber = nums(i)
        	val currentProduct = runningProduct * currentNumber
        	maximumProduct = max(max(currentProduct, maximumProduct), currentProduct / divider)
        	runningProduct = if (currentProduct == 0) 1 else currentProduct
        	if (divider == 1 && currentProduct < 0) {
        		divider = currentProduct
        	}
        	if (currentProduct == 0) {
        		divider = 1
        	}
        	i += 1
        }

        maximumProduct
    }

	def main(args: Array[String]): Unit = {
		val input: Array[Int] = Array(2,-5,-2,-4,3)
		val output: Int = maxProduct(input)
		println(s"Input: ${input mkString ("[", ",", "]")}")
		println(s"Output: $output")
	}
}