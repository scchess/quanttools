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

#ifndef NPERIODS_H
#define NPERIODS_H


inline int NNights( double time1, double time2 ) {

  const int nSecondsInDay = 60 * 60 * 24;

  return std::abs( std::trunc( time1 / nSecondsInDay ) - std::trunc( time2 / nSecondsInDay ) );

}

inline int NDays( double time1, double time2 ) { return NNights( time1, time2 ); }

inline double NHours( double time ) {

  const int nSecondsInHour = 60 * 60;

  return std::fmod( time / nSecondsInHour, 24 );


}

inline double NHours( double time1, double time2 ) {

  const int nSecondsInHour = 60 * 60;

  return std::abs( time2 - time1 ) / nSecondsInHour;

}

#endif //NPERIODS_H
