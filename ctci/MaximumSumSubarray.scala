import scala.math.max

/*Given an integer array nums, find the contiguous subarray (containing at least one number)
 which has the largest sum and return its sum.*/

object MaximumSubarray {
	
	def maxSubArray(nums: Array[Int]): Int = {
		var runningSum = 0
		var maxSum = Int.MinValue
		val volume = nums.length

		var i = 0
		while (i < volume) {
			val num = nums(i)
			val currentSum = runningSum + num
			maxSum = max(currentSum, maxSum)
			runningSum = if (currentSum < 0) 0 else currentSum
			i += 1
		}

		maxSum
	}

	def main(args: Array[String]): Unit = {
		val input: Array[Int] = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
		val output: Int = maxSubArray(input)
		println(s"Input: nums = ${input mkString ("[", ",", "]")}")
		println(s"Output: $output")
	}
}