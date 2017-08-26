package Market

sealed class Profit(buy: Buy, sell: Sell) {
  def cost: Double = buy.total + buy.commission + sell.commission
  def revenue: Double = sell.total
  def profit: Double = revenue - cost
  def profitability: Double = profit/buy.total
}

case class GrossProfit(buy: Buy, sell: Sell) extends Profit(buy: Buy, sell: Sell)

case class NetProfit(buy: Buy, sell: Sell, taxRatio: Double) extends Profit(buy: Buy, sell: Sell) {
  override def profit: Double = {
    val gross = revenue - cost
    gross - gross * taxRatio
  }
}

object Profit {
  def GrossProfit(buy: Buy, sell: Sell): GrossProfit = new GrossProfit(buy, sell)
  def NetProfit(buy: Buy, sell: Sell, taxRatio: Double): NetProfit = new NetProfit(buy, sell, taxRatio)
}
