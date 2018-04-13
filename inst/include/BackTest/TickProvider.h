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

#ifndef TICKPROVIDER_H
#define TICKPROVIDER_H

#include "Tick.h"

class TickProvider {

private:

  Rcpp::NumericVector  bids   ;
  Rcpp::NumericVector  asks   ;
  Rcpp::LogicalVector  systems;
  Rcpp::NumericVector  times  ;
  Rcpp::NumericVector  prices ;
  Rcpp::IntegerVector  volumes;

  int size;
  std::string timeZone;
  bool hasBbo;
  Tick tick;

  bool hasBid;
  bool hasAsk;
  bool hasSystem;

public:

  TickProvider( Rcpp::List ticks ) {

    Rcpp::StringVector names = ticks.attr( "names" );

    bool hasTime   = std::find( names.begin(), names.end(), "time"   ) != names.end();
    bool hasPrice  = std::find( names.begin(), names.end(), "price"  ) != names.end();
    bool hasVolume = std::find( names.begin(), names.end(), "volume" ) != names.end();

    hasBid    = std::find( names.begin(), names.end(), "bid"    ) != names.end();
    hasAsk    = std::find( names.begin(), names.end(), "ask"    ) != names.end();
    hasSystem = std::find( names.begin(), names.end(), "system" ) != names.end();

    if( !hasTime   ) throw std::invalid_argument( "ticks must contain 'time' column"   );
    if( !hasPrice  ) throw std::invalid_argument( "ticks must contain 'price' column"  );
    if( !hasVolume ) throw std::invalid_argument( "ticks must contain 'volume' column" );

    hasBbo = hasBid and hasAsk;

    if( hasBid    ) bids    = ticks[ "bid"    ];
    if( hasAsk    ) asks    = ticks[ "ask"    ];
    if( hasSystem ) systems = ticks[ "system" ];
                    times   = ticks[ "time"   ];
                    prices  = ticks[ "price"  ];
                    volumes = ticks[ "volume" ];

    std::vector<std::string> tzone = times.attr( "tzone" );

    if( tzone.empty() ) throw std::invalid_argument( "ticks timezone must be set" );

    timeZone = tzone[0];

    size = times.size();

  }

  bool HasBbo() { return hasBbo; };
  int Size() { return size; }

  const Tick& Get( int id ) {

      tick.id     = id;
      tick.time   = times  [id];
      tick.price  = prices [id];
      tick.volume = volumes[id];
      if( hasBid    ) tick.bid    = bids   [id];
      if( hasAsk    ) tick.ask    = asks   [id];
      if( hasSystem ) tick.system = systems[id];

      return tick;

  }

};

#endif //TICKPROVIDER_H
