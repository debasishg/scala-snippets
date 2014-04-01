package net.debasishg.snippet.domainpatterns

import java.util.Date
import scalaz._
import Scalaz._
import \/._
import PLens._

object aggregate {
  type ValidationStatus[S] = \/[String, S]
  type ProcessingStatus[S] = \/[String, S]

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

  case class Address(number: String, street: String, city: String, zip: String)
  case class ShipTo(name: String, address: Address)

  case class Order(orderNo: String, orderDate: Date, customer: Customer, 
    lineItems: Vector[LineItem], shipTo: ShipTo, netOrderValue: Option[BigDecimal] = None, status: OrderStatus = Placed)

  /**
   * Specifications
   */
  def isReadyForFulfilment(order: Order) = {
    val s = for {

      _ <- validate
      _ <- approve
      _ <- checkCustomerStatus(order.customer)
      c <- checkInventory

    } yield c
    s(order)
  }

  private def validate = ReaderTStatus[Order, Boolean] {order =>
    if (order.lineItems isEmpty) left(s"Validation failed for order $order") else right(true)
  }

  private def approve = ReaderTStatus[Order, Boolean] {order =>
    println("approved")
    right(true)
  }

  private def checkCustomerStatus(customer: Customer) = ReaderTStatus[Order, Boolean] {order =>
    right(true)
  }

  private def checkInventory = ReaderTStatus[Order, Boolean] {order =>
    println("inventory checked")
    right(true)
  }

  /**
   * lens definitions for update of aggregate root
   */
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

  val orderShipTo = Lens.lensu[Order, ShipTo] (
    (o, sh) => o.copy(shipTo = sh),
    _.shipTo
  )

  val shipToAddress = Lens.lensu[ShipTo, Address] (
    (sh, add) => sh.copy(address = add),
    _.address
  )

  val addressToCity = Lens.lensu[Address, String] (
    (add, c) => add.copy(city = c),
    _.city
  )

  def orderShipToCity = orderShipTo andThen shipToAddress andThen addressToCity
  
  def valueOrder = Kleisli[ProcessingStatus, Order, Order] {order =>
    val o = orderLineItems.set(
      order,
      setLineItemValues(order.lineItems)
    )
    o.lineItems.map(_.value).sequenceU match {
      case Some(_) => right(o)
      case _ => left("Missing value for items")
    }
  }

  private def setLineItemValues(lis: Vector[LineItem]) = {
    (0 to lis.length - 1).foldLeft(lis) {(s, i) => 
      val li = lis(i)
      lineItemValues(i).set(s, unitPrice(li.item).map(_ * li.quantity)).getOrElse(s)
    }
  }

  def applyDiscounts = Kleisli[ProcessingStatus, Order, Order] {order =>
    val o = orderLineItems.set(
      order,
      setLineItemValues(order.lineItems)
    )
    o.lineItems.map(_.discount).sequenceU match {
      case Some(_) => right(o)
      case _ => left("Missing discount for items")
    }
  }

  private def setLineItemDiscounts(lis: Vector[LineItem], customer: Customer) = {
    (0 to lis.length - 1).foldLeft(lis) {(s, i) => 
      val li = lis(i)
      lineItemDiscounts(i).set(s, discount(li.item, customer)).getOrElse(s)
    }
  }

  val orderNetValue = Lens.lensu[Order, Option[BigDecimal]] (
    (o, v) => o.copy(netOrderValue = v),
    _.netOrderValue
  )

  def checkOut = Kleisli[ProcessingStatus, Order, Order] {order =>

    val netOrderValue = order.lineItems.foldLeft(BigDecimal(0).some) {(s, i) => 
      s |+| (i.value |+| i.discount.map(d => Tags.Multiplication(BigDecimal(-1)) |+| Tags.Multiplication(d)))
    }
    right(orderNetValue.set(order, netOrderValue))
  }

  private def unitPrice(item: Item): Option[BigDecimal] = {
    BigDecimal(12).some
  }

  private def discount(item: Item, customer: Customer) = {
    BigDecimal(5).some
  }

  def process(order: Order) = {
    (valueOrder andThen applyDiscounts andThen checkOut) =<< right(orderStatus.set(order, Validated))
  }
}
