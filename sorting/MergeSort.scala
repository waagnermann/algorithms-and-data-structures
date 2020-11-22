object MergeSort {
	/*Merge Sort implementation*/
	private def mergeHalves(leftHalf: List[Int], rightHalf: List[Int]): List[Int] = (leftHalf, rightHalf) match {
		case (Nil, list) => list
		case (list, Nil) => list
		case _ => 
			val leftHead = leftHalf.head
			val rightHead = rightHalf.head
			if (leftHead < rightHead) leftHead :: mergeHalves(leftHalf.tail, rightHalf)
			else rightHead :: mergeHalves(leftHalf, rightHalf.tail)
	}

	private def mergeSort(numbers: List[Int], length: Int): List[Int] = length <= 1 match {
		case true => numbers
		case false =>
			val middle = length / 2
			val (leftHalf, rightHalf) = numbers splitAt middle
			mergeHalves(mergeSort(leftHalf, middle), mergeSort(rightHalf, length - middle))
	}

	def mergeSort(numbers: List[Int]): List[Int] = {
		val length = numbers.length
		mergeSort(numbers, length)
	}


	def main(args: Array[String]): Unit = {
		val numbers: List[Int] = List(3, 7, 8, 5, 2, 1, 9, 5, 4)
		println(mergeSort(numbers))
	}
}