package net.debasishg.snippet.endo

import java.util.{Date, Calendar}
import scalaz._
import Endo._
import scalaz.syntax.all._
import scalaz.syntax.std.all._

object endofluent {
  sealed trait Instrument
  case class Security(isin: String, name: String) extends Instrument

  case class Trade(refNo: String, tradeDate: Date, valueDate: Option[Date] = None, 
    ins: Instrument, principal: BigDecimal, net: Option[BigDecimal] = None, status: TradeStatus = CREATED)

  sealed trait TradeStatus
  case object CREATED extends TradeStatus
  case object FINALIZED extends TradeStatus
  case object VALUE_DATE_ADDED extends TradeStatus
  case object ENRICHED extends TradeStatus
  case object VALIDATED extends TradeStatus

  type TradeLifecycle = Endo[Trade]

  // validate the trade: business logic elided
  def validate: TradeLifecycle = 
    ((t: Trade) => t.copy(status = VALIDATED)).endo

  // add value date to the trade (for settlement)
  def addValueDate: TradeLifecycle = 
    ((t: Trade) => t.copy(valueDate = Some(t.tradeDate), status = VALUE_DATE_ADDED)).endo

  // enrich the trade: add taxes and compute net value: business logic elided
  def enrich: TradeLifecycle = 
    ((t: Trade) => t.copy(net = Some(t.principal + 100), status = ENRICHED)).endo

  // journalize the trade into book: business logic elided
  def journalize: TradeLifecycle = 
    ((t: Trade) => t.copy(status = FINALIZED)).endo

  def doTrade(t: Trade) =
    (journalize |+| enrich |+| addValueDate |+| validate).apply(t)
}


