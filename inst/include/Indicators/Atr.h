// Copyright (C) 2018 Ernesto Fernández
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

#ifndef ATR_H
#define ATR_H

#include "Indicator.h"
#include "Ema.h"
#include "../BackTest/Candle.h"
#include "../ListBuilder.h"
#include <queue>
#include <string>


class AtrValue {

public:

  double trueHigh;
  double trueLow;
  double tr;
  double atr;

};

template< typename Input >
class Atr : public Indicator< Input, AtrValue, Rcpp::List > {

private:

  size_t n;
  size_t counter;

  Ema ema;

  AtrValue info;

  double prevClose;

  std::vector< double > lagCloseHistory;
  std::vector< double > trueHighHistory;
  std::vector< double > trueLowHistory;
  std::vector< double > trHistory;
  std::vector< double > atrHistory;

public:

  Atr( size_t n ) :
  n( n ), ema( ( size_t )n, true )
  {

    if( n < 1 )
      throw std::invalid_argument( "n must be greater than 0" );

    counter = 0;
    prevClose = NAN;

  }

  void Add( Input value )
  {

    info = {};

    if(counter > 0) {
      if(!std::isnan(prevClose) && prevClose > value.high) {
        info.trueHigh = prevClose;
      } else {
        info.trueHigh = value.high;
      }

      if(!std::isnan(prevClose) && prevClose < value.low) {
        info.trueLow = prevClose;
      } else {
        info.trueLow = value.low;
      }

      info.tr = info.trueHigh - info.trueLow;

      ema.Add(info.tr);

      info.atr = ema.GetValue();

    }

    ( counter > 0 ) ? lagCloseHistory.push_back(prevClose) : lagCloseHistory.push_back( NA_REAL );
    ( counter > 0 ) ? trueHighHistory.push_back(info.trueHigh) : trueHighHistory.push_back( NA_REAL );
    ( counter > 0 ) ? trueLowHistory.push_back(info.trueLow) : trueLowHistory.push_back( NA_REAL );
    ( counter > 0 ) ? trHistory.push_back(info.tr) : trHistory.push_back( NA_REAL );
    ( IsFormed() )  ? atrHistory.push_back(info.atr) : atrHistory.push_back( NA_REAL );

    if( counter < n ) counter++;
    prevClose = value.close;
  }

  bool IsFormed() { return ema.IsFormed(); }

  AtrValue GetValue() { return info; }

  Rcpp::List GetHistory() {

  Rcpp::List history = ListBuilder().AsDataTable()
                                    .Add( "tr", trHistory )
                                    .Add( "atr", atrHistory )
                                    .Add( "trueHigh", trueHighHistory )
                                    .Add( "trueLow", trueLowHistory );
    return history;

  }

  void Reset() {

    counter = 0;
    prevClose = NAN;
    ema.Reset();

  }

};

#endif //ATR_H
