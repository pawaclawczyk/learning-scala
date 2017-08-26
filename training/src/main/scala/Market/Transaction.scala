package Market

sealed class Transaction(price: Double, count: Int, commissionRate: Double) {
  def total: Double = price * count
  def commission: Double = commissionRate * total
}

case class Buy(price: Double, count: Int, commissionRate: Double) extends Transaction(price: Double, count: Int, commissionRate: Double)
case class Sell(price: Double, count: Int, commissionRate: Double) extends Transaction(price: Double, count: Int, commissionRate: Double)

object Transaction {
  def Buy(price: Double, count: Int, commissionRate: Double): Buy = new Buy(price: Double, count: Int, commissionRate: Double)
  def Sell(price: Double, count: Int, commissionRate: Double): Sell = new Sell(price: Double, count: Int, commissionRate: Double)
}
