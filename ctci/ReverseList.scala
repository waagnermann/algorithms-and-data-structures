object ReverseList {
	
	def reverse[T](ls: List[T]): List[T] = ls match {
		case Nil => ls
		case list if list.nonEmpty => helper(list, Nil)
	}

	def helper[T](left: List[T], right: List[T]): List[T] = left match {
		case Nil => right
		case head :: tail => helper(tail, head :: right) 
	}
	
	def main(args: Array[String]): Unit = {
		val items: List[Int] = List.range(1, 11)
		val reversedItems: List[Int] = reverse(items)

		println(s"items: $items")
		println(s"reversed items: $reversedItems")
	}
}