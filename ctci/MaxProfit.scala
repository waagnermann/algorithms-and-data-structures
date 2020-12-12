object MaxProfit {
	/*Say you have an array for which the ith element is the price of a given stock on day i.

	If you were only permitted to complete at most one transaction (i.e., buy one and sell one share of the stock),
	design an algorithm to find the maximum profit.

	Note that you cannot sell a stock before you buy one.*/

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
