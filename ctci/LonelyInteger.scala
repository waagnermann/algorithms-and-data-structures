object LonelyInteger {
/* Given an array of integers find one that appears only ince in there. That value will be the lonely integer*/

	def getLonelyInteger[T <: Int](array: Array[T]) = array.foldLeft(0)(_ ^ _)

	def main(args: Array[String]): Unit = {
		val array: Array[Int] = Array(5, 5, 3, 2, 2, 1, 1, 7, 7, 8, 8)

		println(s"Array: ${array mkString ("[", ", ", "]")}")
		println(s"Lonely integer: ${getLonelyInteger(array)}")
	}
}