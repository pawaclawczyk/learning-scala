package Market

import org.scalatest._

class ProfitTest extends FunSuite with Matchers {
  val buy = Buy(2.0, 100, 0.01)
  val sell = Sell(4.0, 100, 0.01)

  test("Create gross profit") {
    GrossProfit(buy, sell) shouldBe a [Profit]
    GrossProfit(buy, sell) shouldBe a [GrossProfit]
    GrossProfit(buy, sell) should not be a [NetProfit]
  }

  test("Create net profit") {
    NetProfit(buy, sell, Rates.taxRate) shouldBe a [Profit]
    NetProfit(buy, sell, Rates.taxRate) shouldBe a [NetProfit]
    NetProfit(buy, sell, Rates.taxRate) should not be a [GrossProfit]
  }

  test("Profit has cost") {
    GrossProfit(buy, sell).cost should be (206.0)
    NetProfit(buy, sell, Rates.taxRate).cost should be (206.0)
  }

  test("Profit has revenue") {
    GrossProfit(buy, sell).revenue should be (400.0)
    NetProfit(buy, sell, Rates.taxRate).revenue should be (400.0)
  }

  test("Gross profit equals revenue minus cost") {
    val expected = 400.0 - 206.0
    GrossProfit(buy, sell).profit should be (expected)
    GrossProfit(buy, sell).profitability should be (expected/ 200.0)
  }

  test("Net profit equals revenue minus cost minus tax") {
    val gross = 400.0 - 206.0
    val expected = gross - Rates.taxRate * gross
    NetProfit(buy, sell, Rates.taxRate).profit should be (expected)
    NetProfit(buy, sell, Rates.taxRate).profitability should be (expected / 200.0)
  }
}
