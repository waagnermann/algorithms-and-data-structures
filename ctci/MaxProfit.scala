object MaxProfit {

	def maxProfit(prices: Array[Int]): Int = {
		val (buyFor, sellFor, profit) = (Int.MaxValue, 0, 0)
		val resultTuple = prices.foldLeft((buyFor, sellFor, profit)){ case ((buyFor, sellFor, profit), price) =>
			if (price <= buyFor) (price, 0, profit)
			else {
				if (price >= sellFor) {
					val diff = price - buyFor
					if (diff >= profit) (buyFor, price, diff)
					else (buyFor, price, profit)
				}
				else (buyFor, sellFor, profit)
			}
		}

		val bestProfit = resultTuple._3
		bestProfit
    }

	def main(args: Array[String]): Unit = {
		val priceList = Array(8, 4, 5, 7, 10, 3, 5, 6)
		println(maxProfit(priceList))
	}
}