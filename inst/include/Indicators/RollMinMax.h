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


// https://www.nayuki.io/page/sliding-window-minimum-maximum-algorithm
#ifndef ROLLMINMAX_H
#define ROLLMINMAX_H

#include <Rcpp.h>

#include "Indicator.h"
#include <vector>
#include <deque>
#include <queue>

class RollMinMax : public Indicator< double, double, std::vector< double > > {

private:

  double extremum;
  size_t n;
  size_t i;
  bool maximize;

  std::deque< double > deque;
  std::queue< double > queue;

  std::vector< double > history;

public:

  RollMinMax( int n, bool maximize ) :
  n( ( size_t )n ),
  maximize( maximize )
  {
    if( n < 1 ) throw std::invalid_argument( "n must be greater than 0" );
    Reset();
  }

  void Add( double value ) {

    bool minimize = not maximize;

    while( not deque.empty() and ( ( minimize and value < deque.back() ) or ( maximize and value > deque.back() ) ) ) {

      deque.pop_back();

    }

    deque.push_back( value );
    queue.push( value );

    if( queue.size() == n ) {

      extremum = deque.front();
      if( queue.front() == deque.front() ) {

        deque.pop_front();

      }
      queue.pop();

    }
    i++;

    IsFormed() ? history.push_back( extremum ) : history.push_back( NA_REAL );

  }

  bool IsFormed() { return i >= n; }

  double GetValue() { return extremum; }

  std::vector< double > GetHistory() { return history; }

  void Reset() {

    std::deque< double > emptyDeque;
    std::swap( deque, emptyDeque );

    std::queue< double > emptyQueue;
    std::swap( queue, emptyQueue );

    i = 0;

  }

};

#endif //ROLLMINMAX_H
