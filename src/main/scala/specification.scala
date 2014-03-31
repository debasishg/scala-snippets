package net.debasishg.snippet.domainpatterns

import java.util.Date
import scalaz._
import Scalaz._
import \/._

object specifications {
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

  case class LineItem(item: Item, quantity: Int)

  case class Order(orderNo: String, orderDate: Date, customer: Customer, lineItems: List[LineItem])

  case class Customer(custId: String, name: String, category: Int)

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
}
