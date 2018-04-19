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

#ifndef ROLLSORTINO_H
#define ROLLSORTINO_H


#include "../Indicators/RollTdd.h"
#include "../Indicators/Sma.h"

class RollSortino : public Indicator< double, double, std::vector<double> > {

private:

  RollTdd tdd;
  Sma mean;
  std::vector< double > history;
  double sortino;
  double T;

public:

  RollSortino( int n, double T ) :
  tdd( n, T ),
  mean( n ),
  T( T )
  {}

  void Add( double value ) {

    mean.Add( value );
    tdd.Add( value );

    sortino = ( mean.GetValue() - T ) / tdd.GetValue();
    tdd.IsFormed() ? history.push_back( GetValue() ) : history.push_back( NA_REAL );

  }

  bool IsFormed() { return tdd.IsFormed(); }

  double GetValue() { return sortino; }

  std::vector<double> GetHistory() { return history; }

  void Reset() {

    tdd.Reset();
    mean.Reset();

  }

};

#endif //ROLLSORTINO_H
