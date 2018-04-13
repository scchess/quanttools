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

#include <Rcpp.h>
#include <vector>
#include "../inst/include/Indicators/CandleAggregator.h"
#include "../inst/include/BackTest/TickProvider.h"
#include "../inst/include/ListBuilder.h"
#include "../inst/include/CppToR.h"

//' Convert ticks to candles
//'
//' @name to_candles
//' @param ticks read 'Ticks' section in \link{Processor}
//' @param timeframe candle timeframe in seconds
//' @return data.table with columns \code{time, open, high, low, close, volume, id}. Where \code{id} is row number of last tick in candle. \cr
//' Note: last candle is always omitted.
//' @rdname to_candles
//' @export
// [[Rcpp::export]]
Rcpp::List to_candles( Rcpp::DataFrame ticks, int timeframe ) {

  TickProvider tickProvider( ticks );
  CandleAggregator candles( timeframe );

  for( int id = 0; id < tickProvider.Size(); id++ ) {

    candles.Add( tickProvider.Get( id ) );

  }

  return candles.GetHistory();

}
