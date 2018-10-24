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

#ifndef PROCESSOR_H
#define PROCESSOR_H

#include <map>
#include <cmath>
#include <vector>
#include <Rcpp.h>
#include "Trade.h"
#include "Order.h"
#include "ProcessorOptions.h"
#include "Tick.h"
#include "TickProvider.h"
#include "Statistics.h"
#include "../CppToR.h"
#include "../ListBuilder.h"
#include "../Alarm.h"
#include "../Utils.h"
#include "../Indicators/CandleAggregator.h"

class Processor {

  friend class Test;

private:

  std::vector< std::string > OrderSideString  = { "buy", "sell" };
  std::vector< std::string > TradeSideString  = { "long", "short" };
  std::vector< std::string > OrderTypeString  = { "market", "limit", "stop", "trail" };
  std::vector< std::string > OrderStateString = { "new", "registered", "executed", "cancelling", "cancelled" };
  std::vector< std::string > TradeStateString = { "new", "opened", "closed", "closing" };

  std::vector<Order*> orders;
  std::vector<Order*> ordersProcessed;

  std::map< int, Trade*> trades;
  std::map< int, Trade*> tradesProcessed;

  CandleAggregator* candleAggregator;

  ProcessorOptions options;

  Alarm alarmMarketOpen;
  Alarm alarmMarketClose;

  Tick prevTick;

  bool isInInterval = false;
  int intervalId = 0;

public:

  Statistics statistics;

  std::function< void( const Tick&   ) > onTick;
  std::function< void( const Candle& ) > onCandle;
  std::function< void( ) > onMarketOpen;
  std::function< void( ) > onMarketClose;
  std::function< void( ) > onIntervalOpen;
  std::function< void( ) > onIntervalClose;

  Processor( int timeFrame, double latencySend = 0.001, double latencyReceive = 0.001 ) :
    statistics( prevTick, options )
  {

    options.latencySend    = latencySend;
    options.latencyReceive = latencyReceive;
    options.timeZone       = "UTC";

    Reset();

    candleAggregator = new CandleAggregator( timeFrame );

  };

  ~Processor() {

    Reset();
    delete candleAggregator;

  }

  void SetCost( Cost cost ) { options.cost = cost; }
  void SetCost( Rcpp::List cost ) {

    Rcpp::StringVector names = cost.attr( "names" );

    bool hasPointValue  = std::find( names.begin(), names.end(), "pointValue"  ) != names.end();
    bool hasCancel      = std::find( names.begin(), names.end(), "cancel"      ) != names.end();
    bool hasOrder       = std::find( names.begin(), names.end(), "order"       ) != names.end();
    bool hasStockAbs    = std::find( names.begin(), names.end(), "stockAbs"    ) != names.end();
    bool hasTradeAbs    = std::find( names.begin(), names.end(), "tradeAbs"    ) != names.end();
    bool hasTradeRel    = std::find( names.begin(), names.end(), "tradeRel"    ) != names.end();
    bool hasLongAbs     = std::find( names.begin(), names.end(), "longAbs"     ) != names.end();
    bool hasLongRel     = std::find( names.begin(), names.end(), "longRel"     ) != names.end();
    bool hasShortAbs    = std::find( names.begin(), names.end(), "shortAbs"    ) != names.end();
    bool hasShortRel    = std::find( names.begin(), names.end(), "shortRel"    ) != names.end();
    bool hasSlippageAbs = std::find( names.begin(), names.end(), "slippageAbs" ) != names.end();
    bool hasSlippageRel = std::find( names.begin(), names.end(), "slippageRel" ) != names.end();

    if( hasPointValue  ) options.cost.pointValue  = cost["pointValue" ];
    if( hasCancel      ) options.cost.cancel      = cost["cancel"     ];
    if( hasOrder       ) options.cost.order       = cost["order"      ];
    if( hasStockAbs    ) options.cost.stockAbs    = cost["stockAbs"   ];
    if( hasTradeAbs    ) options.cost.tradeAbs    = cost["tradeAbs"   ];
    if( hasTradeRel    ) options.cost.tradeRel    = cost["tradeRel"   ];
    if( hasLongAbs     ) options.cost.longAbs     = cost["longAbs"    ];
    if( hasLongRel     ) options.cost.longRel     = cost["longRel"    ];
    if( hasShortAbs    ) options.cost.shortAbs    = cost["shortAbs"   ];
    if( hasShortRel    ) options.cost.shortRel    = cost["shortRel"   ];
    if( hasSlippageAbs ) options.cost.slippageAbs = cost["slippageAbs"];
    if( hasSlippageRel ) options.cost.slippageRel = cost["slippageRel"];

  }

  void SetStop( Rcpp::List stop ) {

    Rcpp::StringVector names = stop.attr( "names" );

    bool hasDrawDown = std::find( names.begin(), names.end(), "drawdown" ) != names.end();
    bool hasLoss	   = std::find( names.begin(), names.end(), "loss"     ) != names.end();
    bool hasTime	   = std::find( names.begin(), names.end(), "time"     ) != names.end();

    if( hasDrawDown ) options.stopTradingDrawdown = stop["drawdown" ];
    if( hasLoss     ) options.stopTradingLoss     = stop["loss"     ];
    if( hasTime     ) options.stopTradingTime     = stop["time"     ];


  }
  void SetTradingHours( double start, double end ) {

    alarmMarketOpen .Set( start );
    alarmMarketClose.Set( end   );

  }
  void SetIntervals( std::vector<double> starts, std::vector<double> ends ) {

    if( starts.size() != ends.size() ) { throw std::invalid_argument( "intervals starts and ends must be the same size" ); }
    options.intervalStarts = starts;
    options.intervalEnds   = ends;

  }
  bool IsTradingHoursSet() { return alarmMarketClose.IsSet() and alarmMarketOpen.IsSet(); }
  void SetLatencyReceive( double latencyReceive ) {

    options.latencyReceive = latencyReceive;

  }
  void SetLatencySend( double latencySend ) {

    options.latencySend = latencySend;

  }
  void SetLatency( double latency ) {

    options.latencySend    = latency / 2;
    options.latencyReceive = latency / 2;

  }
  void SetPriceStep( double priceStep ) {

    options.priceStep = priceStep;

  }
  void SetStartTradingTime( double startTradingTime ) {

    options.startTradingTime = startTradingTime;

  }
  void SetStopTradingTime( double stopTradingTime ) {

    options.stopTradingTime = stopTradingTime;

  }
  void SetExecutionType( ExecutionType executionType ) {

    options.executionType = executionType;

  }
  void SetExecutionType( std::string executionType ) {

    std::map< std::string, ExecutionType > executionTypeMap =
      {
      { "trade", ExecutionType::TRADE },
      { "bbo"  , ExecutionType::BBO   }
      };

    SetExecutionType( executionTypeMap[ executionType ] );

  }
  void AllowLimitToHitMarket() {
    options.allowLimitToHitMarket = true;
  }
  void AllowExactStop() {
    options.allowExactStop = true;
  }

  void SetOptions( Rcpp::List options ) {

    Rcpp::StringVector names = options.attr( "names" );

    bool hasCost           = std::find( names.begin(), names.end(), "cost"            ) != names.end();
    bool hasStop           = std::find( names.begin(), names.end(), "stop"            ) != names.end();
    bool hasTradeStart     = std::find( names.begin(), names.end(), "trade_start"     ) != names.end();
    bool hasLatency        = std::find( names.begin(), names.end(), "latency"         ) != names.end();
    bool hasLatencyReceive = std::find( names.begin(), names.end(), "latency_receive" ) != names.end();
    bool hasLatencySend    = std::find( names.begin(), names.end(), "latency_send"    ) != names.end();
    bool hasTradingHours   = std::find( names.begin(), names.end(), "trading_hours"   ) != names.end();
    bool hasPriceStep      = std::find( names.begin(), names.end(), "price_step"      ) != names.end();
    bool hasExecutionType  = std::find( names.begin(), names.end(), "execution_type"  ) != names.end();
    bool hasIntervals      = std::find( names.begin(), names.end(), "intervals"       ) != names.end();

    bool hasAllowLimitToHitMarket = std::find( names.begin(), names.end(), "allow_limit_to_hit_market"   ) != names.end();
    bool hasAllowExactStop        = std::find( names.begin(), names.end(), "allow_exact_stop"            ) != names.end();

    if( hasTradingHours ) {

      Rcpp::NumericVector tradingHours = options["trading_hours"];
      if( tradingHours.size() != 2 ) { throw std::invalid_argument( "trading_hours must have two elements" ); }
      SetTradingHours( tradingHours[0], tradingHours[1] );

    }

    if( hasCost ) {

      Rcpp::List cost = options["cost"];
      SetCost( cost );

    }
    if( hasStop           ) SetStop            ( options["stop"           ] );
    if( hasTradeStart     ) SetStartTradingTime( options["trade_start"    ] );
    if( hasLatency        ) SetLatency         ( options["latency"        ] );
    if( hasLatencyReceive ) SetLatencyReceive  ( options["latency_receive"] );
    if( hasLatencySend    ) SetLatencySend     ( options["latency_send"   ] );
    if( hasPriceStep      ) SetPriceStep       ( options["price_step"     ] );
    if( hasExecutionType  ) {
      std::string executionType = options["execution_type" ];
      SetExecutionType( executionType );

    }
    if( hasAllowLimitToHitMarket ) if( options["allow_limit_to_hit_market" ] ) AllowLimitToHitMarket();
    if( hasAllowExactStop        ) if( options["allow_exact_stop"          ] ) AllowExactStop       ();
    if( hasIntervals ) {

      Rcpp::List intervals = options[ "intervals" ];
      SetIntervals( intervals[ "start" ], intervals[ "end" ] );

    }

  }


  void Feed( const Tick& tick ) {

    if( tick.time < prevTick.time ) { throw std::invalid_argument( "ticks must be time ordered tick.id = " + std::to_string( tick.id + 1 ) ); }

    if( statistics.drawDown < options.stopTradingDrawdown ) StopTrading();
    if( statistics.marketValue < options.stopTradingLoss )  StopTrading();
    if( tick.time > options.stopTradingTime ) StopTrading();

    if( alarmMarketOpen.GetTime() < alarmMarketClose.GetTime() ) {

      if( onMarketOpen  != nullptr and alarmMarketOpen .IsRinging( tick.time ) ) onMarketOpen();
      if( onMarketClose != nullptr and alarmMarketClose.IsRinging( tick.time ) ) onMarketClose();

    } else {

      if( onMarketClose != nullptr and alarmMarketClose.IsRinging( tick.time ) ) onMarketClose();
      if( onMarketOpen  != nullptr and alarmMarketOpen .IsRinging( tick.time ) ) onMarketOpen();

    }

    if( intervalId < options.intervalEnds.size() ) {

      if( isInInterval ) {

        if( tick.time > options.intervalEnds[ intervalId ] ) {

          if( onIntervalClose != nullptr ) onIntervalClose();
          isInInterval = false;

        }

      } else {

        while( intervalId < options.intervalStarts.size() and options.intervalEnds[ intervalId ] < tick.time ) intervalId++;

        if( intervalId < options.intervalStarts.size() and tick.time > options.intervalStarts[ intervalId ] ) {

          if( onIntervalOpen != nullptr ) onIntervalOpen();
          isInInterval = true;

        }

      }

    }

    candleAggregator->Add( tick );

    if( candleAggregator->IsFormed() ) {

      if( onCandle != nullptr ) onCandle( candleAggregator->GetValue() );

      statistics.SaveOnCandleHistory();

    }

    if( onTick != nullptr and not tick.system ) onTick( tick );

    // allocate memory to prevent iterators invalidation in case some orders have callbacks which send new orders
    auto reserve = orders.size() < 10 ? 20 : orders.size() * 2;
    orders.reserve( reserve );

    for( auto it = orders.begin(); it != orders.end();  ) {

      Order* order = (*it);

      order->Update( prevTick, tick );

      statistics.Update( order );

      bool isOrderInTradesProcessed = tradesProcessed.count( order->idTrade ) != 0;
      bool isOrderInTrades = trades.count( order->idTrade ) != 0;

      if( not ( isOrderInTrades or isOrderInTradesProcessed ) ) {

        trades[ order->idTrade ] = new Trade( order, options );

      } else {

        Trade* trade = isOrderInTradesProcessed ? tradesProcessed[ order->idTrade ] : trades[ order->idTrade ];
        bool tradeIsOpenedOrIsClosing = trade->IsOpened() or trade->IsClosing();

        trade->Update( order );
        if( tradeIsOpenedOrIsClosing and order->IsExecuted() ) statistics.Update( trade );

      }

      if( order->IsExecuted() or order->IsCancelled() ) {

        ordersProcessed.push_back( order );
        it = orders.erase( it );

      } else ++it;

    }

    for( auto it = trades.begin(); it != trades.end();  ) {

      Trade* trade = it->second;

      trade->Update( prevTick, tick );

      if( options.isTradingStopped and trade->IsOpened() and not trade->IsClosing() ) {

        Order* order = trade->Close();

        RegisterOrder( order );

      }

      if( trade->IsClosed() ) {

        tradesProcessed[it->first] = trade;
        it = trades.erase( it );

      } else ++it;

    }

    statistics.Update( prevTick, tick );

    if( not tick.system ) {

      prevTick = tick;

    } else {

      prevTick.time = tick.time;

    }

  }

  Statistics GetStatistics() { return statistics; }
  const Candle& GetCandle() const { return candleAggregator->GetValue(); }

  void StopTrading() {

    options.isTradingStopped = true;
    CancelOrders();

  }

  void Feed( Rcpp::DataFrame ticks ) {

    TickProvider tickProvider( ticks );

    if( options.executionType == ExecutionType::BBO and not tickProvider.HasBbo() ) {

      throw std::invalid_argument( "ticks must contain 'bid' and 'ask' columns" );

    }

    for( int id = 0; id < tickProvider.Size(); id++ ) {

      Feed( tickProvider.Get( id ) );

    }

    statistics.Finalize();

  }

  void SendOrder( Order* order ) {

    if( not CanTrade() ) {

      delete order;
      return;

    }

    RegisterOrder( order );

  }

  void RegisterOrder( Order* order ) {

    if( order->type == OrderType::LIMIT ) {

      if( options.priceStep > 0 ) {

        order->price = ( order->side == OrderSide::BUY ? fastFloor( order->price / options.priceStep ) : fastCeiling( order->price / options.priceStep ) ) * options.priceStep;

      }
      if( options.priceStep < 0 ) {

        order->price = ( order->side == OrderSide::BUY ? fastCeiling( order->price / -options.priceStep ) : fastFloor( order->price / -options.priceStep ) ) * -options.priceStep;

      }

    }

    order->options = &options;

    orders.push_back( order );
    statistics.Update( order );

  }

  void CancelOrders() { for( auto order: orders ) order->Cancel( ); }

  bool CanTrade() { return not( prevTick.time < options.startTradingTime or options.isTradingStopped ); }

  int GetPosition() { return statistics.position; }

  int GetPositionPlanned() { return statistics.positionPlanned; }

  double GetMarketValue() { return statistics.marketValue * 100; }

  void Reset() {

    for( auto order: orders ) delete order;
    orders.clear();

    for( auto order: ordersProcessed ) delete order;
    ordersProcessed.clear();

    for( auto r: trades ) delete r.second;
    trades.clear();

    for( auto r: tradesProcessed ) delete r.second;
    tradesProcessed.clear();

    statistics.Reset();
    prevTick.time = 0;
    options.isTradingStopped = false;

  }

  std::vector<double> GetOnCandleMarketValueHistory() {  return statistics.onCandleHistoryMarketValue;  }

  std::vector<double> GetOnCandleDrawDownHistory() {  return statistics.onCandleHistoryDrawDown;  }

  Rcpp::List GetOnDayClosePerformanceHistory() {

    Rcpp::List performance = ListBuilder().AsDataTable()
                                          .Add( "date"          , IntToDate( statistics.onDayCloseHistoryDates ) )
                                          .Add( "return"        , statistics.onDayCloseHistoryMarketValueChange  )
                                          .Add( "pnl"           , statistics.onDayCloseHistoryMarketValue        )
                                          .Add( "drawdown"      , statistics.onDayCloseHistoryDrawDown           )
                                          .Add( "avg_pnl"       , statistics.onDayCloseHistoryAvgTradePnl        )
                                          .Add( "n_per_day"     , statistics.onDayCloseHistoryNTrades            )
                                          .Add( "n_per_day_long", statistics.onDayCloseHistoryNTradesLong        )
                                          .Add( "close"         , statistics.onDayCloseHistoryClosePrice         );

    return performance;

  }

  Rcpp::List GetCandles() { return candleAggregator->GetHistory(); }

  Rcpp::List GetOrders() {

    int n = orders.size() + ordersProcessed.size();

    Rcpp::IntegerVector   id_trade      ( n );
    Rcpp::IntegerVector   id_sent       ( n );
    Rcpp::IntegerVector   id_processed  ( n );
    Rcpp::NumericVector   time_sent      = DoubleToDateTime( std::vector<double>( n ), options.timeZone );
    Rcpp::NumericVector   time_processed = DoubleToDateTime( std::vector<double>( n ), options.timeZone );
    Rcpp::NumericVector   price_init    ( n );
    Rcpp::NumericVector   price_exec    ( n );
    Rcpp::IntegerVector   side           = IntToFactor( std::vector<int>( n ), OrderSideString );
    Rcpp::IntegerVector   type           = IntToFactor( std::vector<int>( n ), OrderTypeString );
    Rcpp::IntegerVector   state          = IntToFactor( std::vector<int>( n ), OrderStateString );
    Rcpp::CharacterVector comment       ( n );

    int i = 0;
    auto convertOrder = [&]( Order* order ) {

      id_trade      [i] = order->idTrade;
      id_sent       [i] = order->idSent + 1;
      id_processed  [i] = order->idProcessed + 1;
      time_sent     [i] = order->timeSent;
      time_processed[i] = order->timeProcessed;
      price_init    [i] = order->price;
      price_exec    [i] = order->priceExecuted;
      side          [i] = (int)order->side + 1;
      type          [i] = (int)order->type + 1;
      state         [i] = (int)order->state + 1;
      comment       [i] = order->comment;

      i++;

    };

    for( auto it = ordersProcessed.begin(); it != ordersProcessed.end(); it++ ) convertOrder( *it );
    for( auto it = orders         .begin(); it != orders         .end(); it++ ) convertOrder( *it );

    Rcpp::List orders = ListBuilder().AsDataTable()

      .Add( "id_trade"      , id_trade       )
      .Add( "id_sent"       , id_sent        )
      .Add( "id_processed"  , id_processed   )
      .Add( "time_sent"     , time_sent      )
      .Add( "time_processed", time_processed )
      .Add( "price_init"    , price_init     )
      .Add( "price_exec"    , price_exec     )
      .Add( "side"          , side           )
      .Add( "type"          , type           )
      .Add( "state"         , state          )
      .Add( "comment"       , comment        );

      return orders;

  }

  Rcpp::List GetTrades() {

    int n = trades.size() + tradesProcessed.size();

    Rcpp::IntegerVector id_trade   ( n );
    Rcpp::IntegerVector id_sent    ( n );
    Rcpp::IntegerVector id_enter   ( n );
    Rcpp::IntegerVector id_exit    ( n );
    Rcpp::IntegerVector side       = IntToFactor( std::vector<int>( n ), TradeSideString );
    Rcpp::NumericVector price_enter( n );
    Rcpp::NumericVector price_exit ( n );
    Rcpp::NumericVector time_sent  = DoubleToDateTime( std::vector<double>( n ), options.timeZone );
    Rcpp::NumericVector time_enter = DoubleToDateTime( std::vector<double>( n ), options.timeZone );
    Rcpp::NumericVector time_exit  = DoubleToDateTime( std::vector<double>( n ), options.timeZone );
    Rcpp::NumericVector pnl        ( n );
    Rcpp::NumericVector mtm        ( n );
    Rcpp::NumericVector mtm_min    ( n );
    Rcpp::NumericVector mtm_max    ( n );
    Rcpp::NumericVector cost       ( n );
    Rcpp::NumericVector pnl_rel    ( n );
    Rcpp::NumericVector mtm_min_rel( n );
    Rcpp::NumericVector mtm_max_rel( n );
    Rcpp::NumericVector mtm_rel    ( n );
    Rcpp::NumericVector cost_rel   ( n );
    Rcpp::IntegerVector state      = IntToFactor( std::vector<int>( n ), TradeStateString );

    const int basisPoints = 10000;

    int i = 0;
    auto convertTrade = [&]( Trade* trade ) {

      id_trade   [i] = trade->idTrade;
      id_sent    [i] = trade->idSent  + 1;
      id_enter   [i] = trade->idEnter + 1;
      id_exit    [i] = trade->idExit  + 1;
      time_sent  [i] = trade->timeSent;
      time_enter [i] = trade->timeEnter;
      time_exit  [i] = trade->timeExit;
      side       [i] = (int)trade->side + 1;
      price_enter[i] = trade->priceEnter;
      price_exit [i] = trade->priceExit;
      pnl        [i] = trade->pnl;
      mtm        [i] = trade->mtm;
      mtm_min    [i] = trade->mtmMin;
      mtm_max    [i] = trade->mtmMax;
      cost       [i] = trade->cost;
      pnl_rel    [i] = trade->pnlRel    * basisPoints;
      mtm_rel    [i] = trade->mtmRel    * basisPoints;
      mtm_min_rel[i] = trade->mtmMinRel * basisPoints;
      mtm_max_rel[i] = trade->mtmMaxRel * basisPoints;
      cost_rel   [i] = trade->costRel   * basisPoints;
      state      [i] = (int)trade->state + 1;

      i++;

    };

    for( auto it = tradesProcessed.begin(); it != tradesProcessed.end(); it++ ) convertTrade( it->second );
    for( auto it = trades         .begin(); it != trades         .end(); it++ ) convertTrade( it->second );

    Rcpp::List trades = ListBuilder().AsDataTable()

      .Add( "id_trade"   , id_trade    )
      .Add( "id_sent"    , id_sent     )
      .Add( "id_enter"   , id_enter    )
      .Add( "id_exit"    , id_exit     )
      .Add( "time_sent"  , time_sent   )
      .Add( "time_enter" , time_enter  )
      .Add( "time_exit"  , time_exit   )
      .Add( "side"       , side        )
      .Add( "price_enter", price_enter )
      .Add( "price_exit" , price_exit  )
      .Add( "pnl"        , pnl         )
      .Add( "mtm"        , mtm         )
      .Add( "mtm_min"    , mtm_min     )
      .Add( "mtm_max"    , mtm_max     )
      .Add( "cost"       , cost        )
      .Add( "pnl_rel"    , pnl_rel     )
      .Add( "mtm_rel"    , mtm_rel     )
      .Add( "mtm_min_rel", mtm_min_rel )
      .Add( "mtm_max_rel", mtm_max_rel )
      .Add( "cost_rel"   , cost_rel    )
      .Add( "state"      , state       );

      return trades;

  }

  Rcpp::List GetSummary() { return statistics.GetSummary(); }

};

#endif //PROCESSOR_H
