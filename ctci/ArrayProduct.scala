object ArrayProduct {
	/*Given an array nums of n integers where n > 1,  
	return an array output such that output[i] is equal 
	to the product of all the elements of nums except nums[i].*/

	def productExceptSelf(nums: Array[Int]): Array[Int] = {
        val len = nums.length
        val result = Array.ofDim[Int](len)
        result(0) = 1

        var index = 1
        while (index < len) {
        	result(index) = nums(index - 1) * result(index - 1)
        	index += 1
        }

        var rightProduct = 1
        index = len - 1
        while (index >= 0) {
        	result(index) = rightProduct * result(index)
        	rightProduct *= nums(index)
        	index -= 1
        }

        result
	}

	def main(args: Array[String]): Unit = {
		val input: Array[Int] = Array(1, 2, 3, 4)
		val output: Array[Int] = productExceptSelf(input)
		println(output mkString("[", ", ", "]"))
	}
}