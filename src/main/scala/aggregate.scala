package net.debasishg.snippet.domainpatterns

import java.util.Date
import scalaz._
import Scalaz._
import \/._
import PLens._

object aggregate {
  type ValidationStatus[S] = \/[String, S]

  type ReaderTStatus[A, S] = ReaderT[ValidationStatus, A, S]

  object ReaderTStatus extends KleisliInstances with KleisliFunctions {
    def apply[A, S](f: A => ValidationStatus[S]): ReaderTStatus[A, S] = kleisli(f)
  }

  sealed trait Item {
    def itemCode: String
  }
  case class ItemA(itemCode: String, desc: Option[String], minPurchaseUnit: Int) extends Item
  case class ItemB(itemCode: String, desc: Option[String], nutritionInfo: String) extends Item

  case class LineItem(item: Item, quantity: BigDecimal, value: Option[BigDecimal] = None, discount: Option[BigDecimal] = None)

  case class Customer(custId: String, name: String, category: Int)

  sealed trait OrderStatus
  case object Placed extends OrderStatus
  case object Validated extends OrderStatus

  case class Order(orderNo: String, orderDate: Date, customer: Customer, 
    lineItems: Vector[LineItem], status: OrderStatus = Placed)


  def isReadyForFulfilment(order: Order) = {
    val s = for {

      _ <- validate
      _ <- approve
      _ <- checkCustomerStatus(order.customer)
      c <- checkInventory

    } yield c
    s(order)
  }

  private def validate = ReaderTStatus[Order, Boolean] {case order =>
    if (order.lineItems isEmpty) left(s"Validation failed for order $order") else right(true)
  }

  private def approve = ReaderTStatus[Order, Boolean] {case order =>
    println("approved")
    right(true)
  }

  private def applyPromotions(customer: Customer) = ReaderTStatus[Order, Boolean] {case order =>
    println("promotions applied")
    right(true)
  }

  private def checkCustomerStatus(customer: Customer) = ReaderTStatus[Order, Boolean] {case order =>
    right(true)
  }

  private def checkInventory = ReaderTStatus[Order, Boolean] {case order =>
    println("inventory checked")
    right(true)
  }

  private def checkOut = ReaderTStatus[Order, Boolean] {case order =>
    println("checked out")
    right(true)
  }

  val orderStatus = Lens.lensu[Order, OrderStatus] (
    (o, value) => o.copy(status = value),
    _.status
  )

  val orderLineItems = Lens.lensu[Order, Vector[LineItem]] (
    (o, lis) => o.copy(lineItems = lis),
    _.lineItems
  )

  val lineItemValue = Lens.lensu[LineItem, Option[BigDecimal]] (
    (l, v) => l.copy(value = v),
    _.value
  )

  val lineItemDiscount = Lens.lensu[LineItem, Option[BigDecimal]] (
    (l, value) => l.copy(discount = value),
    _.discount
  )

  def lineItemValues(i: Int) = ~lineItemValue compose vectorNthPLens(i)
  def lineItemDiscounts(i: Int) = ~lineItemDiscount compose vectorNthPLens(i)

  def valueOrder(order: Order): Order = {
    orderLineItems.set(
      order,
      setLineItemValues(order.lineItems)
    )
  }

  private def setLineItemValues(lis: Vector[LineItem]) = {
    (0 to lis.length - 1).foldLeft(lis) {(s, i) => 
      val li = lis(i)
      lineItemValues(i).set(s, unitPrice(li.item).map(_ * li.quantity)).getOrElse(Vector.empty[LineItem])
    }
  }

  def applyDiscounts(order: Order): Order = {
    orderLineItems.set(
      order,
      setLineItemValues(order.lineItems)
    )
  }

  private def setLineItemDiscounts(lis: Vector[LineItem], customer: Customer) = {
    (0 to lis.length - 1).foldLeft(lis) {(s, i) => 
      val li = lis(i)
      lineItemDiscounts(i).set(s, discount(li.item, customer)).getOrElse(Vector.empty[LineItem])
    }
  }

  private def unitPrice(item: Item): Option[BigDecimal] = {
    BigDecimal(12).some
  }

  private def discount(item: Item, customer: Customer) = {
    BigDecimal(5).some
  }
}
