import java.util.{Date, Calendar}
object cake {

  sealed trait Currency
  case object USD extends Currency
  case object EUR extends Currency
  case object AUD extends Currency

  val baseCurrency: Currency = USD

  val baseCurrencyFactor: Map[Currency, Double] = Map(USD -> 1, EUR -> 1.3, AUD -> 1.2)

  case class Account(no: String, name: String, openedOn: Date, status: String)

  trait BalanceComponent {
    type Balance

    def balance(amount: Double, currency: Currency, asOf: Date): Balance
    def inBaseCurrency(b: Balance): Balance
  }

  trait SimpleBalanceComponent extends BalanceComponent {
    type Balance = (Double, Currency, Date)
    override def balance(amount: Double, currency: Currency, asOf: Date) = (amount, currency, asOf)
    override def inBaseCurrency(b: Balance) = 
      ((b._1) * baseCurrencyFactor.get(b._2).get, baseCurrency, b._3)
  }

  trait CustomBalanceComponent extends BalanceComponent {
    type Balance = BalanceRep
    case class BalanceRep(amount: Double, currency: Currency, asOf: Date)
    override def balance(amount: Double, currency: Currency, asOf: Date) = 
      BalanceRep(amount, currency, asOf)
    override def inBaseCurrency(b: Balance) = 
      BalanceRep((b.amount) * baseCurrencyFactor.get(b.currency).get, baseCurrency, b.asOf)
  }

  trait Portfolio {
    val bal: BalanceComponent
    import bal._

    def currentPortfolio(account: Account): List[Balance]
  }

  trait ClientPortfolio extends Portfolio {
    val bal: BalanceComponent
    import bal._

    override def currentPortfolio(account: Account) = {
      //.. actual impl will fetch from database
      List(
        balance(1000, EUR, Calendar.getInstance.getTime),
        balance(1500, AUD, Calendar.getInstance.getTime)
      )
    }
  }

  trait Auditing extends Portfolio {
    val semantics: Portfolio
    val bal: semantics.bal.type
    import bal._

    override def currentPortfolio(account: Account) = {
      semantics.currentPortfolio(account) map inBaseCurrency
    }
  }

  object SimpleBalanceComponent extends SimpleBalanceComponent
  object CustomBalanceComponent extends CustomBalanceComponent

  object ClientPortfolioAuditService1 extends Auditing {
    val semantics = new ClientPortfolio { val bal = SimpleBalanceComponent }
    val bal: semantics.bal.type = semantics.bal
  }

  object ClientPortfolioAuditService2 extends Auditing {
    val semantics = new ClientPortfolio { val bal = CustomBalanceComponent }
    val bal: semantics.bal.type = semantics.bal
  }
}

