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

#ifndef STATISTICS_H
#define STATISTICS_H

#include "Order.h"
#include "Trade.h"
#include "Candle.h"
#include "Tick.h"
#include "ProcessorOptions.h"
#include "../CppToR.h"
#include "../ListBuilder.h"
#include "../NPeriods.h"
#include "../Indicators/RollSharpe.h"
#include "../Indicators/RollSortino.h"
#include "../Indicators/RollLinReg.h"
#include <cmath>
#include <vector>
#include <Rcpp.h>

class Statistics {

  friend class Processor;

public:

  double testStart;
  double testEnd;
  int    nDaysTested;
  int    nDaysTraded;
  double nTradesPerDay;
  int    nTradesTotal;
  int    nTradesLong;
  int    nTradesShort;
  int    nTradesWin;
  int    nTradesLoss;
  double pTradesWin;
  double pTradesLoss;
  double avgTradeWin;
  double avgTradeLoss;
  double avgTradePnl;
  double totalWin;
  double totalLoss;
  double totalPnl;
  double maxDrawDown;
  double maxDrawDownStart;
  double maxDrawDownEnd;
  double maxDrawDownLength;
  double sharpe;
  double sortino;
  double rSquared;
  double avgDrawDown;

private:

  int    positionPlanned;
  int    position;
  double positionValue;

  double drawDown;
  double drawDownStart;
  double drawDownEnd;
  double marketValue;
  double marketValueMax;
  double lastPrice;
  bool   isDrawDownMax;

  std::vector<double> onDayCloseHistoryMarketValue;
  std::vector<double> onDayCloseHistoryMarketValueChange;
  std::vector<double> onDayCloseHistoryDrawDown;
  std::vector<int>    onDayCloseHistoryDates;
  std::vector<int>    onDayCloseHistoryNTrades;
  std::vector<int>    onDayCloseHistoryNTradesLong;
  std::vector<double> onDayCloseHistoryAvgTradePnl;
  std::vector<double> onDayCloseHistoryClosePrice;

  int onDayCloseNTrades;
  int onDayCloseNTradesLong;
  double onDayCloseTradePnl;

  std::vector<double> onCandleHistoryMarketValue;
  std::vector<double> onCandleHistoryDrawDown;

  int date;

  RollSortino rollSortino;
  RollSharpe  rollSharpe;
  RollLinReg  rollRegression;

  const ProcessorOptions& options;
  const Tick& prevTick;

public:

  int nTradingDaysInYear = 252;

public:

  Statistics( const Tick& prevTick, const ProcessorOptions& options ) : prevTick( prevTick ), options( options ), rollSortino( 100000, 0. ), rollSharpe( 100000 ), rollRegression( 100000 ) { Reset(); }

  void Reset() {

    testStart         = NAN;
    testEnd           = NAN;
    nDaysTested       = 0;
    nDaysTraded       = 0;
    nTradesPerDay     = 0;
    nTradesTotal      = 0;
    nTradesLong       = 0;
    nTradesShort      = 0;
    nTradesWin        = 0;
    nTradesLoss       = 0;
    pTradesWin        = 0;
    pTradesLoss       = 0;
    avgTradeWin       = 0;
    avgTradeLoss      = 0;
    avgTradePnl       = 0;
    totalWin          = 0;
    totalLoss         = 0;
    totalPnl          = 0;
    maxDrawDown       = 0;
    maxDrawDownStart  = NAN;
    maxDrawDownEnd    = NAN;
    maxDrawDownLength = NAN;
    sharpe            = NAN;
    sortino           = NAN;
    rSquared          = NAN;
    avgDrawDown       = 0;

    positionPlanned   = 0;
    position          = 0;
    positionValue     = 1;

    drawDown          = 0;
    drawDownStart     = NAN;
    drawDownEnd       = NAN;
    marketValue       = 0;
    marketValueMax    = 0;

    isDrawDownMax     = false;

    onDayCloseHistoryMarketValue      .clear();
    onDayCloseHistoryMarketValueChange.clear();
    onDayCloseHistoryDrawDown         .clear();
    onDayCloseHistoryDates            .clear();
    onDayCloseHistoryAvgTradePnl      .clear();
    onDayCloseHistoryNTrades          .clear();
    onDayCloseHistoryNTradesLong      .clear();
    onDayCloseHistoryClosePrice       .clear();
    onDayCloseNTrades     = 0;
    onDayCloseNTradesLong = 0;
    onDayCloseTradePnl    = 0;

    onCandleHistoryMarketValue   .clear();
    onCandleHistoryDrawDown      .clear();

    date = 0;

    rollRegression.Reset();
    rollSharpe    .Reset();
    rollSortino   .Reset();

  }

  void Update( Order* order ) {

    if( order->IsNew() ) {

      positionPlanned += order->side == OrderSide::BUY ? +1 : -1;

    }

    if( order->IsExecuted() ) {

      if( order->side == OrderSide::BUY ) {

        if( position == -1 ) positionValue = order->priceExecuted;
        if( position >=  0 ) positionValue = ( positionValue * position + order->priceExecuted ) / ( position + 1 );
        position++;
        positionPlanned--;

      } else {

        if( position ==  1 ) positionValue = order->priceExecuted;
        if( position <=  0 ) positionValue = ( positionValue * position - order->priceExecuted ) / ( position - 1 );
        position--;
        positionPlanned++;

      }

      int date = order->timeExecuted / nSecondsInDay;

      if( date != this->date ) {

        this->date = date;
        nDaysTraded++;

      }

    }

    if( order->IsCancelled() ) {

      order->side == OrderSide::BUY ? positionPlanned-- : positionPlanned++;

    }

  }

  void Update( Trade* trade ) {

    if( not trade->IsClosed() ) return;

    nTradesTotal++;
    avgTradePnl = ( avgTradePnl * ( nTradesTotal - 1 ) + trade->pnlRel ) / nTradesTotal;
    totalPnl += trade->pnlRel;
    nTradesPerDay = nTradesTotal * 1.0 / nDaysTraded;

    if( trade->pnlRel > 0 ) {

      nTradesWin++;
      avgTradeWin = ( avgTradeWin * ( nTradesWin - 1 ) + trade->pnlRel ) / nTradesWin;
      totalWin += trade->pnlRel;

    } else {

      nTradesLoss++;
      avgTradeLoss = ( avgTradeLoss * ( nTradesLoss - 1 ) + trade->pnlRel ) / nTradesLoss;
      totalLoss += trade->pnlRel;

    }
    pTradesWin  = nTradesWin  * 1.0 / nTradesTotal;
    pTradesLoss = nTradesLoss * 1.0 / nTradesTotal;

    if( trade->IsLong() ) {

      nTradesLong++;
      onDayCloseNTradesLong++;

    } else {

      nTradesShort++;

    }

    onDayCloseTradePnl += trade->pnlRel;
    onDayCloseNTrades ++;

  }

  void onDayStart() { // previous day close

    onDayCloseHistoryDates.push_back( prevTick.time / nSecondsInDay );

    double marketValueChange = nDaysTested == 0 ? marketValue : marketValue - onDayCloseHistoryMarketValue.back();

    onDayCloseHistoryMarketValueChange.push_back( marketValueChange );
    onDayCloseHistoryMarketValue      .push_back( marketValue );
    onDayCloseHistoryDrawDown         .push_back( drawDown );
    onDayCloseHistoryAvgTradePnl      .push_back( onDayCloseNTrades == 0 ? 0 : onDayCloseTradePnl / onDayCloseNTrades );
    onDayCloseHistoryNTrades          .push_back( onDayCloseNTrades );
    onDayCloseHistoryNTradesLong      .push_back( onDayCloseNTradesLong );
    onDayCloseHistoryClosePrice       .push_back( lastPrice );
    nDaysTested++;

    rollSharpe    .Add( marketValueChange );
    rollSortino   .Add( marketValueChange );
    rollRegression.Add( std::make_pair( nDaysTested, marketValue ) );

    sharpe   = rollSharpe    .GetValue() * std::sqrt( nTradingDaysInYear );
    sortino  = rollSortino   .GetValue() * std::sqrt( nTradingDaysInYear );
    rSquared = rollRegression.GetValue().rSquared;

    avgDrawDown = ( avgDrawDown * ( nDaysTested - 1 ) + drawDown ) / nDaysTested;

    onDayCloseNTrades     = 0;
    onDayCloseNTradesLong = 0;
    onDayCloseTradePnl    = 0;

  }

  void Finalize() { onDayStart(); }

  void Update( const Tick& prevTick, const Tick& tick ) {

    if( prevTick.time == 0 ) { testStart = tick.time; } else {

      if( NNights( prevTick.time, tick.time ) > 0 ) onDayStart();

    }

    if( not tick.system ) {

      if( options.executionType == ExecutionType::TRADE ) {

        marketValue = totalPnl + position * ( tick.price / positionValue - 1 );

      }
      if( options.executionType == ExecutionType::BBO ) {

        marketValue = totalPnl + position * ( ( position > 0 ? prevTick.bid : prevTick.ask ) / positionValue - 1 );

      }

    }

    if( marketValueMax < marketValue ) marketValueMax = marketValue;

    double prevDrowdown = drawDown;
    drawDown = marketValue - marketValueMax;
    lastPrice = tick.price;

    bool isDrawDownStarted = prevDrowdown == 0 and drawDown < 0;
    if( isDrawDownStarted ) {

      drawDownStart = tick.time;
      drawDownEnd   = NAN;

    }

    bool isDrawDownEnded = prevDrowdown < 0 and drawDown == 0;
    if( isDrawDownEnded ) {

      drawDownEnd = tick.time;

    }

    bool isMaxDrawdownRecovered = isDrawDownEnded and isDrawDownMax;
    if( isMaxDrawdownRecovered ) {

      maxDrawDownEnd    = drawDownEnd;
      maxDrawDownLength = NNights( maxDrawDownStart, maxDrawDownEnd );
      isDrawDownMax     = false;

    }

    bool isMaxDrawDownUpdated = drawDown < maxDrawDown;
    if( isMaxDrawDownUpdated ) {

      maxDrawDown       = drawDown;
      maxDrawDownStart  = drawDownStart;
      maxDrawDownEnd    = NAN;
      maxDrawDownLength = NAN;
      isDrawDownMax     = true;

    }

    testEnd = tick.time;

  }

  void SaveOnCandleHistory() {

    if( std::isnan( marketValue ) ) {

      onCandleHistoryMarketValue.push_back( 0 );
      onCandleHistoryDrawDown.push_back( 0 );

    } else {

      onCandleHistoryMarketValue.push_back( marketValue );
      onCandleHistoryDrawDown.push_back( drawDown );

    }

  }

  Rcpp::List GetSummary() {

    double percents    = 100;
    double basisPoints = 10000;
    double epsilon     = 0.01;

    Rcpp::List summary = ListBuilder().AsDataTable()

      .Add( "from"          , DoubleToDateTime( testStart, options.timeZone )              )
      .Add( "to"            , DoubleToDateTime( testEnd  , options.timeZone )              )
      .Add( "days_tested"   , nDaysTested                                                  )
      .Add( "days_traded"   , nDaysTraded                                                  )
      .Add( "n_per_day"     , std::round( nTradesPerDay / epsilon ) * epsilon              )
      .Add( "n"             , nTradesTotal                                                 )
      .Add( "n_long"        , nTradesLong                                                  )
      .Add( "n_short"       , nTradesShort                                                 )
      .Add( "n_win"         , nTradesWin                                                   )
      .Add( "n_loss"        , nTradesLoss                                                  )
      .Add( "pct_win"       , std::round( pTradesWin   * percents    / epsilon ) * epsilon )
      .Add( "pct_loss"      , std::round( pTradesLoss  * percents    / epsilon ) * epsilon )
      .Add( "avg_win"       , std::round( avgTradeWin  * basisPoints / epsilon ) * epsilon )
      .Add( "avg_loss"      , std::round( avgTradeLoss * basisPoints / epsilon ) * epsilon )
      .Add( "avg_pnl"       , std::round( avgTradePnl  * basisPoints / epsilon ) * epsilon )
      .Add( "win"           , std::round( totalWin     * percents    / epsilon ) * epsilon )
      .Add( "loss"          , std::round( totalLoss    * percents    / epsilon ) * epsilon )
      .Add( "pnl"           , std::round( totalPnl     * percents    / epsilon ) * epsilon )
      .Add( "max_dd"        , std::round( maxDrawDown  * percents    / epsilon ) * epsilon )
      .Add( "max_dd_start"  , DoubleToDateTime( maxDrawDownStart, options.timeZone )       )
      .Add( "max_dd_end"    , DoubleToDateTime( maxDrawDownEnd  , options.timeZone )       )
      .Add( "max_dd_length" , maxDrawDownLength                                            )
      .Add( "sharpe"        , std::round( sharpe   / epsilon ) * epsilon                   )
      .Add( "sortino"       , std::round( sortino  / epsilon ) * epsilon                   )
      .Add( "r_squared"     , std::round( rSquared / epsilon ) * epsilon                   )
      .Add( "avg_dd"        , std::round( avgDrawDown  * percents / epsilon ) * epsilon    );

      return summary;

  }

};

#endif //STATISTICS_H
