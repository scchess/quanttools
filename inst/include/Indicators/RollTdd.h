// Copyright (C) 2018 Stanislav Kovalevsky
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

// http://www.cmegroup.com/education/files/sortino-a-sharper-ratio.pdf

#ifndef ROLLTDD_H
#define ROLLTDD_H

#include <queue>
#include <vector>
#include <stdexcept>
#include <cmath>
#include "Rcpp.h"
#include "Indicator.h"

class RollTdd : public Indicator< double, double, std::vector<double> > {

private:

  double sumXX;

  double tdd;

  double T;

  std::size_t n;
  std::size_t i;

  std::queue< double > window;

  std::vector< double > history;

public:

  RollTdd( int n, double T ) :
  n( ( std::size_t )n ), T( T )
  {
    if( n < 2 ) throw std::invalid_argument( "n must be greater than 1" );
    sumXX = 0;
    i     = 0;
  }

  void Add( double value ) {

    value = value > 0 ? 0 : value - T;

    sumXX += value * value;
    i++;
    window.push( value );

    if( window.size() > n ) {

      double old = window.front();

      window.pop();

      sumXX -= old * old;
      i--;

    }
    tdd = std::sqrt( sumXX / i );

    IsFormed() ? history.push_back( GetValue() ) : history.push_back( NA_REAL );

  }

  bool IsFormed() { return window.size() == n; }

  double GetValue() { return tdd; }

  std::vector<double> GetHistory() { return history; }

  void Reset() {

    std::queue<double> empty;
    std::swap( window, empty );
    sumXX = 0;
    i     = 0;

  }

};

#endif //ROLLTDD_H
