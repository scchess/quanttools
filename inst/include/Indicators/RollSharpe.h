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

#ifndef ROLLSHARPE_H
#define ROLLSHARPE_H

#include "../Indicators/RollSd.h"
#include "../Indicators/Sma.h"

class RollSharpe : public Indicator< double, double, std::vector<double> > {

private:

  RollSd sd;
  Sma mean;
  std::vector< double > history;
  double sharpe;

public:

  RollSharpe( int n ) :
  sd( n ),
  mean( n )
  {}

  void Add( double value ) {

    mean.Add( value );
    sd.Add( value );

    sharpe = mean.GetValue() / sd.GetValue();
    sd.IsFormed() ? history.push_back( GetValue() ) : history.push_back( NA_REAL );

  }

  bool IsFormed() { return sd.IsFormed(); }

  double GetValue() { return sharpe; }

  std::vector<double> GetHistory() { return history; }

  void Reset() {

    sd.Reset();
    mean.Reset();

  }

};

#endif //ROLLSHARPE_H
