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

#ifndef PROCESSOROPTIONS_H
#define PROCESSOROPTIONS_H

#include "Cost.h"

enum class ExecutionType: int { TRADE, BBO };

class ProcessorOptions {

  public:

  double latencySend;
  double latencyReceive;

  Cost   cost;

  double        startTradingTime      = 0;
  double        stopTradingTime       = 3155760000; // 2070-01-01 00:00:00
  double        stopTradingDrawdown   = NAN;
  double        stopTradingLoss       = NAN;
  bool          isTradingStopped      = false;
  bool          allowLimitToHitMarket = false;
  bool          allowExactStop        = false;
  double        priceStep             = 0;
  ExecutionType executionType         = ExecutionType::TRADE;

  std::vector<double> intervalStarts;
  std::vector<double> intervalEnds;

  std::string timeZone;

};

#endif //PROCESSOROPTIONS_H
