package Market

import org.scalatest._

class TransactionTest extends FunSuite with Matchers {
  test("Create buy transaction") {
    Buy(1.00, 100, 0.2) shouldBe a [Transaction]
    Buy(1.00, 100, 0.2) shouldBe a [Buy]
    Buy(1.00, 100, 0.2) should not be a [Sell]
  }

  test("Create sell transaction") {
    Sell(1.00, 100, 0.2) shouldBe a [Transaction]
    Sell(1.00, 100, 0.2) shouldBe a [Sell]
    Sell(1.00, 100, 0.2) should not be a [Buy]
  }

  test("Transaction has total value") {
    Buy(1.00, 100, 0.2).total should equal (100.0)
    Sell(1.00, 100, 0.2).total should equal (100.0)
  }

  test("Transaction has commission") {
    Buy(1.00, 100, 0.2).commission should equal (20.0)
    Sell(1.00, 100, 0.2).commission should equal (20.0)
  }
}
