// Copyright (C) 2016 Stanislav Kovalevsky
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

#ifndef CANDLEAGGREGATOR_H
#define CANDLEAGGREGATOR_H

#include "Indicator.h"
#include "../CppToR.h"
#include "../ListBuilder.h"
#include "../BackTest/Tick.h"
#include "../BackTest/Candle.h"

class CandleAggregator : public Indicator< const Tick&, const Candle&, Rcpp::List > {

private:

  Candle candleCurrent;
  Candle candlePrevious;

  std::vector< int    > historyId;
  std::vector< double > historyOpen;
  std::vector< double > historyHigh;
  std::vector< double > historyLow;
  std::vector< double > historyClose;
  std::vector< double > historyTime;
  std::vector< double > historyVolume;

  int timeFrame;

  bool startOver;

public:

  CandleAggregator( int timeFrame ) :
  candleCurrent ( timeFrame  ),
  candlePrevious( timeFrame  ),
  timeFrame     ( timeFrame  )
  {

    Reset();

  }

  void Add( const Tick& tick ) {

    double time = floor( tick.time / timeFrame ) * timeFrame + timeFrame;

    startOver = time != candleCurrent.time and candleCurrent.time != 0;

    if( startOver or candleCurrent.time == 0 ) {

      if( candleCurrent.time != 0 ) {

        candlePrevious = candleCurrent;
        historyId    .push_back( candlePrevious.id + 1 );
        historyOpen  .push_back( candlePrevious.open   );
        historyHigh  .push_back( candlePrevious.high   );
        historyLow   .push_back( candlePrevious.low    );
        historyClose .push_back( candlePrevious.close  );
        historyTime  .push_back( candlePrevious.time   );
        historyVolume.push_back( candlePrevious.volume );

      }

      candleCurrent.time = time;
      candleCurrent.id   = tick.id;

      if( not tick.system ) {

        candleCurrent.open   = tick.price;
        candleCurrent.high   = tick.price;
        candleCurrent.low    = tick.price;
        candleCurrent.close  = tick.price;
        candleCurrent.volume = tick.volume;

        candleCurrent.isEmpty = false;

      } else {

        candleCurrent.open   = NAN;
        candleCurrent.high   = NAN;
        candleCurrent.low    = NAN;
        candleCurrent.close  = NAN;
        candleCurrent.volume = 0;

        candleCurrent.isEmpty = true;

      }

    } else {

      candleCurrent.id = tick.id;

      if( not tick.system ) {

        if( candleCurrent.isEmpty ) {

          candleCurrent.open   = tick.price;
          candleCurrent.high   = tick.price;
          candleCurrent.low    = tick.price;
          candleCurrent.close  = tick.price;
          candleCurrent.volume = tick.volume;

          candleCurrent.isEmpty = false;

        } else {

          candleCurrent.close = tick.price;
          candleCurrent.volume += tick.volume;

          if( candleCurrent.high < tick.price ) candleCurrent.high = tick.price;
          if( candleCurrent.low  > tick.price ) candleCurrent.low  = tick.price;

        }

      }

    }

  }

  bool IsFormed() { return startOver; }

  const Candle& GetValue() { return candlePrevious; }

  Rcpp::List GetHistory() {

    Rcpp::List candles = ListBuilder().AsDataTable()

    .Add( "time"  , DoubleToDateTime( historyTime, "UTC" ) )
    .Add( "open"  , historyOpen   )
    .Add( "high"  , historyHigh   )
    .Add( "low"   , historyLow    )
    .Add( "close" , historyClose  )
    .Add( "volume", historyVolume )
    .Add( "id"    , historyId     );

    return( candles );

  }

  void Reset() {

    candleCurrent .isEmpty   = true;
    candlePrevious.isEmpty   = true;

    startOver = false;

  }

};

#endif //CANDLEAGGREGATOR_H
