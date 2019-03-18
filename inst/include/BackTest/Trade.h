// Copyright (C) 2016-2018 Stanislav Kovalevsky
//
// This file is part of QuantTools.
//
// QuantTools is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// QuantTools is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with QuantTools. If not, see <http://www.gnu.org/licenses/>.

#ifndef TRADE_H
#define TRADE_H

#include "Order.h"
#include "ProcessorOptions.h"
#include "../NPeriods.h"

enum class TradeSide: int { LONG, SHORT };

enum class TradeState: int { NEW, OPENED, CLOSED, CLOSING };

class Trade {

  friend class Statistics;
  friend class Processor;

    TradeState state;
    int    idTrade    = NA_INTEGER - 1;
    int    idSent     = NA_INTEGER - 1;
    int    idEnter    = NA_INTEGER - 1;
    int    idExit     = NA_INTEGER - 1;
    TradeSide side;
    double priceEnter = NA_REAL;
    double priceExit  = NA_REAL;
    double timeSent   = NA_REAL;
    double timeEnter  = NA_REAL;
    double timeExit   = NA_REAL;
    double pnl        = NA_REAL;
    double mtm        = 0;
    double mtmMin     = 0;
    double mtmMax     = 0;
    double cost       = 0;
    double pnlRel     = NA_REAL;
    double mtmRel     = 0;
    double mtmMinRel  = 0;
    double mtmMaxRel  = 0;
    double costRel    = 0;

    const ProcessorOptions& options;

    bool IsOpened() { return state == TradeState::OPENED; }
    bool IsClosed() { return state == TradeState::CLOSED; }
    bool IsClosing(){ return state == TradeState::CLOSING;}
    bool IsNew()    { return state == TradeState::NEW;    }
    bool IsLong()   { return side  == TradeSide::LONG;    }
    bool IsShort()  { return side  == TradeSide::SHORT;   }

    Trade( const Order* order, const ProcessorOptions& options ) :
      options( options ){

      idTrade  = order->idTrade;
      state    = TradeState::NEW;
      idSent   = order->idSent;
      timeSent = order->timeSent;
      cost     = options.cost.order;
      side     = order->IsBuy() ? TradeSide::LONG : TradeSide::SHORT; // planned

    }

    void Update( const Order* order ) {

      if( order->IsExecuted() ) {

        cost += options.cost.stockAbs + options.cost.tradeAbs + options.cost.slippageAbs + ( options.cost.tradeRel + options.cost.slippageRel ) * order->priceExecuted * options.cost.pointValue;

        if( IsOpened() or IsClosing() ) {

          idExit    = order->idProcessed;
          timeExit  = order->timeProcessed;
          priceExit = order->priceExecuted;
          pnl       = ( IsLong() ? +1. : -1. ) * ( priceExit - priceEnter ) * options.cost.pointValue + cost;
          pnlRel    = pnl / ( priceEnter * options.cost.pointValue );
          state     = TradeState::CLOSED;

        }

        if( IsNew() ) {

          idEnter    = order->idProcessed;
          timeEnter  = order->timeProcessed;
          priceEnter = order->priceExecuted;

          state = TradeState::OPENED;

          side  = order->IsBuy() ? TradeSide::LONG : TradeSide::SHORT; // actual

        }

      }

      if( order->IsNew()       ) { cost += options.cost.order;  }
      if( order->IsCancelled() ) { cost += options.cost.cancel; }

      costRel = cost / ( priceEnter * options.cost.pointValue );

    }

    void Update( const Tick& prevTick, const Tick& tick ) {

      if( not IsOpened() ) return;

      int nNights = NNights( prevTick.time, tick.time );

      if( nNights > 0 ) {

        cost += nNights * ( IsLong() ? options.cost.longAbs : options.cost.shortAbs );
        cost += nNights * ( IsLong() ? options.cost.longRel : options.cost.shortRel ) * prevTick.price * options.cost.pointValue;

      }

      if( not tick.system ) {

        if( options.executionType == ExecutionType::TRADE ) {
          mtm = ( IsLong() ? +1. : -1. ) * ( tick.price - priceEnter );
        }
        if( options.executionType == ExecutionType::BBO ) {
          mtm = ( IsLong() ? prevTick.bid - priceEnter : priceEnter - prevTick.ask );
        }

      }

      mtmRel = mtm / priceEnter;

      if( mtmMax < mtm ) {

        mtmMax    = mtm;
        mtmMaxRel = mtmRel;

      }
      if( mtmMin > mtm ) {

        mtmMin    = mtm;
        mtmMinRel = mtmRel;

      }

    }

    Order* Close() {

      if( not IsClosing() ) {

        state = TradeState::CLOSING;

        Order* order = new Order( IsLong() ? OrderSide::SELL : OrderSide::BUY, OrderType::MARKET, NA_REAL, "stop", idTrade );

        return order;

      }

      return nullptr;

    }

};

#endif //TRADE_H
