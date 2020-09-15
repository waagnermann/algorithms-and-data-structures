object PowersOf2 extends App {
	
	def powersOf2(n: Int): Int = {
	if (n < 1) {
		0
	} else if (n == 1) {
		println(1)
		1
	} else {
		val prev = powersOf2(n / 2)
		val curr = prev * 2
		println(curr)
		curr
	}
}
	
	powersOf2(256)
}