#####################
## Bollinger Bands ##
#####################

# load tick data
data( 'ticks' )

# define strategy
strategy_source = system.file( package = 'QuantTools', 'examples/bbands.cpp' )
# compile strategy
Rcpp::sourceCpp( strategy_source )

# set strategy parameters
parameters = data.table(
  n         = 100,
  k         = 0.5,
  timeframe = 60
)

# set options, see 'Options' section
options = list(
  cost    = list( tradeAbs = -0.01 ),
  latency = 0.1 # 100 milliseconds
)

# run test
test_summary = bbands( ticks, parameters, options, fast = TRUE )
print( test_summary )

# run test
test = bbands( ticks, parameters, options, fast = FALSE )

# plot result
indicators = plot_dts(
  test$indicators,
  test$orders[ side == 'buy' , .( time_processed, buy  = price_exec ) ],
  test$orders[ side == 'sell', .( time_processed, sell = price_exec ) ] )$
  add_candles( type = 'candlestick', monochromatic = F, gap = 0.5 )$
  add_lines( c( 'lower', 'sma', 'upper' ), c( 'Lower', 'SMA', 'Upper' ) )$
  add_lines( c( 'buy', 'sell' ), type = 'p', pch = c( 24, 25 ), col = c( 'blue', 'red' ) )

performance = plot_dts( test$indicators[, .( time, pnl = pnl * 100, drawdown = drawdown * 100 ) ] )$
  add_lines( c( 'pnl', 'drawdown' ), c( 'P&L, %', 'Draw Down, %' ), col = c( 'darkolivegreen', 'darkred' ) )

interval = '2016-01-19 12/13'
par( mfrow = c( 2, 1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 2, 0 ) )
indicators $set_limits( tlim = interval )$set_style( time_axis = list( visible = F ) )
title( 'Indicators' , adj = 0 )
performance$set_limits( tlim = interval )
title( 'Performance', adj = 0 )
par( mfrow = c( 1, 1 ), oma = c( 0, 0, 0, 0 ), mar = c( 5, 4, 4, 2 ) + 0.1 )
