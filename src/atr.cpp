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

#include "../inst/include/Indicators/Atr.h"
#include <string>

//' Average True Range
//'
//' @name atr
//' @param x \code{high, low, close} data.frame
//' @param n window size
//' @family technical indicators
//' @return data.table with columns \code{tr, atr, trueHigh, trueLow}
//' @description Average True Rangeis a measure of volatility of a High-Low-Close series.
//' @export
// [[Rcpp::export]]
Rcpp::List atr( Rcpp::DataFrame x, size_t n ) {
  Rcpp::DataFrame hlc = Rcpp::as< Rcpp::DataFrame >( x );
  Rcpp::StringVector names = hlc.attr( "names" );

  bool hasHigh   = std::find( names.begin(), names.end(), "high"   ) != names.end();
  bool hasLow  = std::find( names.begin(), names.end(), "low"  ) != names.end();
  bool hasClose = std::find( names.begin(), names.end(), "close" ) != names.end();

  if( !hasHigh   ) throw std::invalid_argument( "ticks must contain 'high' column"   );
  if( !hasLow  ) throw std::invalid_argument( "ticks must contain 'low' column"  );
  if( !hasClose ) throw std::invalid_argument( "ticks must contain 'close' column" );

  Rcpp::NumericVector highs  = hlc[ "high"   ];
  Rcpp::NumericVector lows   = hlc[ "low"    ];
  Rcpp::NumericVector closes = hlc[ "close"  ];

  Atr<Candle> atr( n );

  for( auto i = 0; i < highs.size(); i++ ) {
    Candle candle( 1 );
    candle.low   = lows  [i];
    candle.high  = highs [i];
    candle.close = closes[i];

    atr.Add( candle );
  }

  return atr.GetHistory();
}
//' @name atr
