object FindMinimumRotatedSortedArray {
/*Suppose an array of length n sorted in ascending order is rotated between 1 and n times. 
For example, the array nums = [0,1,2,4,5,6,7] might become:

    [4,5,6,7,0,1,2] if it was rotated 4 times.
    [0,1,2,4,5,6,7] if it was rotated 7 times.

Notice that rotating an array [a[0], a[1], a[2], ..., a[n-1]] 1 time
 results in the array [a[n-1], a[0], a[1], a[2], ..., a[n-2]].

Given the sorted rotated array nums, return the minimum element of this array.*/

	def findMin(nums: Array[Int]): Int = {
		val first = nums(0)
		val last = nums(nums.length - 1)
		
		if (first <= last) first
		else {
			val midIdx = nums.length / 2
			if (midIdx == nums.length - 1) last else {
				val middle = nums(midIdx)
				if (middle <= last) findMin(nums.take(midIdx + 1)) else findMin(nums.drop(midIdx + 1))
			}
		}
	}

	def main(args: Array[String]): Unit = {
		val input = Array(5, 6, 7, 8, 1, 2, 3, 4)
		val output = findMin(input)
		println(s"input: ${input mkString("[", ", ", "]")}")
		println(s"output: $output")
	}
}