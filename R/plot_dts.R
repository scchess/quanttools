# Copyright (C) 2019 Stanislav Kovalevsky
#
# This file is part of QuantTools.
#
# QuantTools is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# QuantTools is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with QuantTools. If not, see <http://www.gnu.org/licenses/>.

#' Plot data.table time series
#' @section Methods:
#' \describe{
#'  \item{\bold{\code{plot_dts}}}{
#'  Add data to be plotted.
#'  }

#'  \item{\bold{\code{$lines}}}{
#'  Add lines with following arguments:
#'  \tabular{ll}{
#'     \code{names}                        \tab vector of column names to plot                        \cr
#'     \code{labels}                       \tab vector of labels if different from column names       \cr
#'     \code{type}                         \tab vector or single value, see \link[graphics]{lines}    \cr
#'     \code{lty,pch,col,lwd,lend}         \tab vector or single value, see \link[graphics]{par}      \cr
#'     \code{bg}                           \tab vector or single value, see \link[graphics]{points}   \cr
#'  }
#'  }
#'  \item{\bold{\code{$events}}}{
#'  Add vertical events lines with following arguments:
#'  \tabular{ll}{
#'     \code{data}                         \tab two column data.table date/time index + event id      \cr
#'     \code{names}                        \tab only 'auto' supported                                 \cr
#'     \code{labels}                       \tab vector of labels                                      \cr
#'     \code{lty,col,lwd}                  \tab vector or single value, see \link[graphics]{par}      \cr
#'  }
#'  }
#'  \item{\bold{\code{$candles}}}{
#'  Add candles with following arguments:
#'     \tabular{ll}{
#'     \code{ohlc}                         \tab vector of open, high, low and close names             \cr
#'     \code{timeframe}                    \tab candle timeframe in minutes for intraday candles      \cr
#'     \code{position}                     \tab relative to time position only \code{'end'} supported \cr
#'     \code{type}                         \tab \code{'barchart'} or \code{'candlestick'}             \cr
#'     \code{gap}                          \tab gap between candles in fraction of \code{width}       \cr
#'     \code{mono}                         \tab should all candles have same color?                   \cr
#'     \code{col,col_up,col_flat,col_down} \tab colors                                                \cr
#'  }
#'  }
#'  \item{\bold{\code{$limits}}}{
#'  \tabular{ll}{
#'     \code{xlim}                         \tab vector of length two to limit plot area horizontally  \cr
#'     \code{ylim}                         \tab vector of length two to limit plot area vertically    \cr
#'     \code{tlim}                         \tab date or time vector of length two                     \cr
#'     \code{time_range}                   \tab intraday time limit in format 'H:M:S/H:M:S'           \cr
#'  }
#'  }
#'  \item{\bold{\code{$style}}}{
#'  Change default plot options. Available options are:
#'  \tabular{lll}{
#'     \bold{\code{grid}}\tab \tab \cr
#'     \code{minute}      \tab \code{list(col,lty)} \tab minute vertical gridline color and line type \cr
#'     \code{hour}        \tab \code{list(col,lty)} \tab hour vertical gridline color and line type   \cr
#'     \code{day}         \tab \code{list(col,lty)} \tab day vertical gridline color and line type    \cr
#'     \code{month}       \tab \code{list(col,lty)} \tab month vertical gridline color and line type  \cr
#'     \code{year}        \tab \code{list(col,lty)} \tab year vertical gridline color and line type   \cr
#'     \code{zero}        \tab \code{list(col,lty)} \tab zero horizontal gridline color and line type \cr
#'     \bold{\code{time}}\tab \tab \cr
#'     \code{grid}        \tab \code{logical}   \tab should vertical gridlines be plotted?                 \cr
#'     \code{resolution}  \tab \code{character} \tab auto, minute, hour, day, month, year or years         \cr
#'     \code{round}       \tab \code{numeric}   \tab time axis rounding in minutes                         \cr
#'     \code{visible}     \tab \code{logical}   \tab should time axis be plotted?                          \cr
#'     \bold{\code{value}}\tab \tab \cr
#'     \code{grid}        \tab \code{logical}  \tab should horizontal gridlines be plotted? \cr
#'     \code{last}        \tab \code{logical}  \tab should last values be plotted?          \cr
#'     \code{log}         \tab \code{logical}  \tab should y axis be in logarithmic scale?  \cr
#'     \code{visible}     \tab \code{logical}  \tab should y axis be plotted?               \cr
#'     \bold{\code{candle}}\tab \tab \cr
#'     \code{auto}          \tab \code{logical}                 \tab shoud candles be automatically detected and plotted?  \cr
#'     \code{col}           \tab \code{list(mono,up,flat,down)} \tab colors                                                \cr
#'     \code{gap}           \tab \code{numeric}                 \tab gap between candles in fraction of \code{width}       \cr
#'     \code{mono}          \tab \code{logical}                 \tab should all candles have same color?                   \cr
#'     \code{position}      \tab \code{character}               \tab relative to time position only \code{'end'} supported \cr
#'     \code{type}          \tab \code{character}               \tab \code{'candlestick'} or \code{'barchart'}             \cr
#'     \bold{\code{line}}\tab \tab \cr
#'     \code{auto}          \tab \code{logical}                 \tab shoud lines be automatically detected and plotted?    \cr
#'     \bold{\code{legend}}\tab \tab \cr
#'     \code{col}           \tab \code{list(background,frame)} \tab colors                       \cr
#'     \code{horizontal}    \tab \code{logical}                \tab should legend be horizontal? \cr
#'     \code{inset}         \tab \code{numeric}                \tab see \link[graphics]{legend}  \cr
#'     \code{position}      \tab \code{character}              \tab see \link[graphics]{legend}  \cr
#'     \code{position_event}\tab \code{character}              \tab see \link[graphics]{legend}  \cr
#'     \code{visible}       \tab \code{logical}                \tab should legend be plotted?    \cr
#'  }
#'  }
#' }
#'
#' @usage NULL
#'
#' @examples
#' \donttest{
#'
#' ## Data
#'
#' aapl_candles = get_yahoo_data                ( 'AAPL', '2014-01-01', '2015-01-01' )
#' aapl_events  = get_yahoo_splits_and_dividends( 'AAPL', '2014-01-01', '2015-01-01' )
#'
#' msft_candles = get_yahoo_data                ( 'MSFT', '2014-01-01', '2015-01-01' )
#' msft_events  = get_yahoo_splits_and_dividends( 'MSFT', '2014-01-01', '2015-01-01' )
#'
#' # change system margins ( to revert use par( margins_old ) )
#' margins_old = par( oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ) )
#'
#'
#' ## Plots
#'
#' # Default:
#' p = plot_dts( aapl_candles[, .( date, close ) ] )
#' p$plot()
#'
#' # User defined names and line types:
#' plot_dts( aapl_candles[, .( date, close ) ] )$
#'   lines( names = 'close', labels = 'Apple', col = 'purple', lwd = 1, lty = 2, type = 's' )$
#'   plot()
#'
#' # With events:
#' plot_dts( aapl_candles[, .( date, close ) ], aapl_events[, .( date, event ) ] )$
#'   lines( names = 'close', labels = 'Apple', col = 'purple', lwd = 1, lty = 5, type = 'l' )$
#'   events( name = 'event' )$
#'   plot()
#'
#' # With labeled events and user defined style:
#' plot_dts( aapl_candles[, .( date, close ) ], aapl_events[, .( date, event ) ] )$
#'   lines( names = 'close', labels = 'Apple', col = 'purple', lwd = 1, lty = 5, type = 'l' )$
#'   events( name = 'event', label = 'dividends',
#'     labels = format( aapl_events$value, digits = 3 ), lty = 4, col = 'red' )$
#'   plot()
#'
#' # Multiple data sets:
#' plot_dts(
#'   aapl_candles[, .( date, aapl = close / close[1] ) ],
#'   msft_candles[, .( date, msft = close / close[1] ) ] )$
#'   lines( names = c( 'aapl', 'msft' ), labels = c( 'Apple', 'Microsoft' ), lty = c( 1, 2 ) )$
#'   plot()
#'
#' # Candles auto detection:
#' plot_dts( aapl_candles[, .( date, open, high, low, close ) ] )$
#'   plot()
#'
#' # User defined candles with user defined style:
#' plot_dts( aapl_candles[, .( date, o = open, h = high, l = low, c = close ) ] )$
#'   candles( names = c( 'o', 'h', 'l', 'c' ), type = 'candlestick', gap = 0.5 )$
#'   plot()
#'
#' # Other methods:
#' plot_dts( aapl_candles[, .( date, open, high, low, close, sma = sma( close, 20 ) ) ] )$
#'   candles()$
#'   lines( names = 'sma', labels = 'SMA( close ) 20', lwd = 2 )$
#'   style(
#'     candle = list( type = 'candlestick', gap = 0.5 ),
#'     time = list( resolution = 'day' ) )$
#'   limits( tlim = '2014-07' )$
#'   plot()
#'
#' # Grid:
#' prices  = plot_dts(
#'   aapl_candles[, .( date, Apple = close / close[1] ) ],
#'   msft_candles[, .( date, Microsoft = close / close[1] ) ] )
#' volume = plot_dts(
#'   aapl_candles[, .( date, Apple = volume * close / 1e9 ) ],
#'   msft_candles[, .( date, Microsoft = volume * close / 1e9 ) ] )$
#'   lines( type = 'h' )
#'
#' pars = par( mfrow = c( 2, 1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ) )
#' prices$style( time = list( visible = F ), legend = list( col = list( text = 'black', background = rgb( 1,1,1,0.5 ) ) ) )$
#'   plot()
#' volume$style( legend = list( visible = F ), legend = list( col = list( text = 'black', background = rgb( 1,1,1,0.5 ) ) ) )$
#'   plot()
#' par( pars )
#'
#' # Histogram:
#' volume = merge(
#'
#'   aapl_candles[, .( date, aapl = volume * close / 1e9 ) ],
#'   msft_candles[, .( date, msft = volume * close / 1e9 ) ],
#'   by = 'date', all = T
#'
#' )
#'
#' volume[ is.na( volume ) ] = 0
#'
#' # Side by side
#' plot_dts( volume )$
#'   histogram( c( 'aapl', 'msft' ), labels = c( 'Apple', 'Microsoft' ), split = T )$limits( tlim = '2014-07' )$
#'   plot()
#'
#' # Stacked
#' volume_stacked = volume[][, ( 2:3 ) := tlt( .SD, cumsum ), .SDcols = 2:3 ][]
#'
#' plot_dts( volume_stacked )$
#'   histogram( c( 'aapl', 'msft' ), labels = c( 'Apple', 'Microsoft' ), stacked = T )$limits( tlim = '2014-07' )$
#'   plot()
#'
#' # Stacked normalized with other parameters
#' volume_norm = volume_stacked[][, ( 2:3 ) := .SD / .SD[[2]], .SDcols = 2:3 ][]
#'
#' plot_dts( volume_norm )$
#'   histogram( c( 'aapl', 'msft' ), labels = c( 'Apple', 'Microsoft' ), stacked = T, gap = 0, border = 'white' )$limits( tlim = '2014-07' )$
#'   plot()
#'
#' # theme
#' theme_old = par( bg = 'black', col = 'white', col.axis = 'white', col.lab = 'white', col.main = 'white' )
#'
#' prices  = plot_dts(
#'   aapl_candles[, .( date, Apple = close / close[1] ) ],
#'   msft_candles[, .( date, Microsoft = close / close[1] ) ] )
#' volume = plot_dts(
#'   aapl_candles[, .( date, Apple = volume * close / 1e9 ) ],
#'   msft_candles[, .( date, Microsoft = volume * close / 1e9 ) ] )$
#'   lines( type = 'h' )
#'
#' pars = par( mfrow = c( 2, 1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ) )
#' prices$style( time = list( visible = F ), legend = list( col = list( text = 'black', background = rgb( 1,1,1,0.5 ) ) ) )$
#'   plot()
#' volume$style( legend = list( visible = F ), legend = list( col = list( text = 'black', background = rgb( 1,1,1,0.5 ) ) ) )$
#'   plot()
#' par( pars )
#'
#' par( theme_old )
#'
#' # revert system margins
#' par( margins_old )
#'
#' }
#'
#' @export
plot_dts = function( ... ) {

   PlotDts$new( ... )

}

plot.PlotTs = function( x ) x$plot()

PlotDts <- R6::R6Class( 'PlotDts', lock_objects = F )

PlotDts$set( 'public', 'initialize', function( ... ) {

  private$data = list( ... )

  if( length( private$data ) == 0 ) stop( 'nothing to plot' )

  private$data_summary = private$screen_data( private$data )

  private$basis_type = if( private$data_summary[ class == 'POSIXct' & col_id == 1, .N > 0 ] ) 'POSIXct' else 'Date'

  private$t_to_x_map = private$data_summary[ class == private$basis_type ][ , .( t = private$data[[ id ]][[ col_id ]] ), by = .( id, col_id ) ]

  private$basis = private$calc_basis( private$t_to_x_map$t )

  private$t_to_x_map[, x := self$t_to_x( t ) ]

  self$style_info = list(

    grid = list(

      minute = list( col = 'grey95', lty = 3 ),
      hour   = list( col = 'grey90', lty = 3 ),
      day    = list( col = 'grey80', lty = 3 ),
      month  = list( col = 'grey70', lty = 3 ),
      year   = list( col = 'grey0' , lty = 3 ),
      zero   = list( col = 'red'   , lty = 3 )

    ),

    time = list(

      round      = 15,
      resolution = 'auto',
      visible    = TRUE,
      grid       = TRUE

    ),

    value = list(

      log     = FALSE,
      visible = TRUE,
      grid    = TRUE,
      last    = TRUE,
      format  = list( digits = 3, nsmall = 0, big.mark = ' ' )

    ),

    candle = list(

      auto     = TRUE,
      position = 'end', # 'middle', 'start'
      type     = 'barchart', # candlestick
      gap      = 0,
      mono     = TRUE,
      col      = list(

        mono = 'steelblue',
        up   = 'steelblue',
        flat = 'yellowgreen',
        down = 'tomato'

      )

    ),

    line = list(

      auto = TRUE

    ),

    legend = list(

      position = 'topleft',
      position_event = 'bottomleft',
      visible  = TRUE,
      inset    = 0.01,
      col      = list(

        background = par( 'bg' ),
        frame      = NA,
        text       = par( 'col' )

      ),
      horizontal = FALSE

    )
  )

  invisible( self )

} )

PlotDts$set( 'private', 'calc_basis', function( t, gap = 5, fixed = T, range = NULL, units = 'mins' ) {

  type = class( t )[1]

  t = sort( t )

  switch(
    type,
    POSIXct = {

      basis = data.table( t )[, .( t_from = as.numeric( t[1] - date, units = units ), t_to = as.numeric( t[.N] - date, units = units ) ), by = .( date = round_POSIXct( t, 24, units = 'hours', trunc ) ) ]

      if( fixed ) {

        basis[-1 , t_from := min( t_from ) ]
        basis[-.N, t_to   := max( t_to   ) ]

      }

      if( !is.null( range ) ) {

        range = as.numeric( as.difftime( range ), units = units )
        basis[, ':='( t_from = t_range[1], t_to = t_range[2] ) ]

      }

      # round to gap
      basis[, t_from := floor  ( t_from / gap ) * gap - gap ]
      basis[, t_to   := ceiling( t_to   / gap ) * gap       ]
      basis[ t_from < 0, t_from := 0 ]

      basis[, x_to     := cumsum( t_to - t_from ) ]
      basis[, x_from   := shift( x_to, fill = 0 ) ]

      setattr( basis, 'units', units )
      setattr( basis, 'step', gap )

    },
    Date = {

      basis = data.table( date = unique( t ) )[ , .( date, x = 1:.N, x_from = 1:.N, x_to = 1:.N ) ]
      setattr( basis, 'step', 1 )

    }
  )

  setattr( basis, 'type', type )
  basis[]

} )

PlotDts$set( 'private', 'screen_data', function( data ) {

  summary = lapply( data, function( x ) {

    sapply( lapply( x, is ), head, 1 )

  } )
  summary = rbindlist( lapply( summary, as.data.table, keep.rownames = T ), idcol = 'id' )
  setnames( summary, c( 'id', 'name', 'class' ) )
  summary[, col_id := 1:.N, by = id ]

  summary

} )

PlotDts$set( 'private', 'subset_basis', function( b, tlim ) {

  switch(
    attr( b, 'type' ),
    POSIXct = {

      dlim = round_POSIXct( tlim, 24, units = 'hours', trunc )

      basis = b[ date %bw% dlim ]

      if( nrow( basis ) == 0 ) return( NULL )

      times = as.numeric( tlim - dlim, units = attr( b, 'units' ) )

      xlim = basis[, c( x_from[1] + ( times[1] - t_from[1] ), x_to[.N] - ( t_to[.N] - times[2] ) ) ]

      basis[ 1 , x_from := xlim[1] ]
      basis[ .N, x_to   := xlim[2] ]

      basis[ 1 , t_from := times[1] ]
      basis[ .N, t_to   := times[2] ]

      basis = basis[ t_from < t_to ]

    },
    Date = {

      basis = b[ date %bw% tlim ]

    } )

    if( nrow( basis ) == 0 ) return( NULL )
    basis

} )

PlotDts$set( 'public', 'style', function( ... ) {

  args = list(...)
  #if( is.null( names( args ) ) ) args = args[[1]]
  self$style_info = modifyList( self$style_info, args )

  invisible( self )

} )

PlotDts$set( 'public', 'print', function( ... ) {

  self$plot()

} )

PlotDts$set( 'public', 'limits', function( tlim = NULL, ylim = NULL ) {

  private$set_limits( tlim, ylim )

  invisible( self )

} )

PlotDts$set( 'private', 'set_limits', function( tlim = NULL, ylim = NULL ) {

  no_tasks_available = length( private$tasks ) == 0
  if( no_tasks_available ) {

    names = private$data_summary[ class %in% c( 'numeric', 'double' ), name ]

    candles_detected = length( names ) == 4 && all( names == c( 'open', 'high', 'low', 'close' ) )

    if( candles_detected ) self$candles() else self$lines()

  }

  private$basis_limited = private$basis
  if( !is.null( tlim ) ) private$basis_limited = private$subset_basis( private$basis, tlim )

  private$t_to_x_map[ , visible := t %bw% tlim ]

  names = unlist( lapply( private$tasks, '[[', 'names' ) )

  if( uniqueN( names ) != length( names ) ) stop( 'duplicated column names not allowed', call. = F )

  absent_names = names %w/o% private$data_summary$name
  numeric_names = names[ names %in% private$data_summary[ class %in% c( 'numeric', 'integer' ) , name ] ]

  if( length( absent_names ) > 0 ) stop( 'following column names are absent in data: ', paste( absent_names, collapse = ',' ), call. = F )
  if( length( numeric_names ) == 0 ) stop( 'please set at least one column name for numeric data', call. = F )

  private$data_summary[
    name %in% names & class %in% c( 'numeric', 'integer' ),
    c( 'min', 'max', 'last' ) := if( .N > 0 ) {

      y = na.omit( private$data[[ id ]][[ col_id ]][ private$get_visible( id ) ] )
      as.list( c( range( y ), tail( y, 1 ) ) )

    }, by = .( id, col_id ) ]

  if( is.null( ylim ) ) {

    private$ylim = private$data_summary[ name %in% names & class %in% c( 'numeric', 'integer' ), if( .N > 0 ) range( min, max, self$ylim ) ]

  } else {

    private$ylim = ylim

  }

  names = unlist( lapply( private$tasks, function( task ) switch( task$type , candles = task$names[4], task$names ) ) )

  private$last = private$data_summary[ name %in% names & class %in% c( 'numeric', 'integer' ), last ]

  private$xlim = private$basis_limited[, c( x_from[1], x_to[.N] ) ] + attr( private$basis_limited, 'step' ) * c( -1, 0 )

} )

PlotDts$set( 'public', 't_to_x', function( t ) {

  switch(
    attr( private$basis, 'type' ),
    POSIXct = {

      private$basis[ cut( t, c( date, Inf ), labels = F ), x_from + { t = as.numeric( t - date, units = attr( private$basis, 'units' ) ); t[ t < t_from | t > t_to ] = NA; t - t_from } ]

    },
    Date = {

      private$basis[ match( t, date ), x ]

    }
  )

} )

PlotDts$set( 'private', 'calc_resolution', function( b ) {

  switch(
    attr( b, 'type' ),
    POSIXct = {
      time_span_hours = b[, as.numeric( as.difftime( sum( x_to - x_from ), units = attr( b, 'units' ) ), units = 'hours' ) ]
      time_span_dates = b[, .N ]
    },
    Date = {
      time_span_dates = max( b[, .N ], 9 )
    }
  )

  if( time_span_dates      > 1500 ) 'years' else
    if( time_span_dates      > 100  ) 'year'  else
      if( time_span_dates      > 50   ) 'month' else
        if( time_span_dates      > 8    ) 'day'   else
          if( time_span_hours      > 4    ) 'hour'  else
            'minute'

} )

PlotDts$set( 'private', 'calc_intraday_marks', function( b, step ) {

  step = as.numeric( step, units = attr( b, 'units' ) )

  b[, .( t = date + as.difftime( c( t_from, seq( ceiling( t_from / step ) * step, floor( t_to / step ) * step + step, step ) ), units = attr( b, 'units' ) ) ), by = date ][, x := self$t_to_x( t ) ][ !rev( duplicated( rev( x ) ) ) ][]

} )

PlotDts$set( 'private', 'plot_basis', function() {

  b = private$basis_limited

  xlim = private$xlim
  ylim = private$ylim

  plot( 1, 1, type = 'n', xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xaxs = 'i', bty = 'n', log = if( self$style_info$value$log ) 'y' else ''  )

  if( self$style_info$time$resolution == 'auto' ) self$style_info$time$resolution = private$calc_resolution( private$basis_limited )

  private$plot_time_axis_and_grid()
  private$plot_value_axis_and_grid()

} )

PlotDts$set( 'private', 'plot_value_axis_and_grid', function() {

  ax_ticks = round( axTicks( 2 ), 10 )

  if( self$style_info$value$grid ) {

    if( self$x_grid$hours ) abline( h = ax_ticks, col = self$style_info$grid$hour $col, lty = self$style_info$grid$hour $lty ) else
    if( self$x_grid$dates ) abline( h = ax_ticks, col = self$style_info$grid$day  $col, lty = self$style_info$grid$day  $lty ) else
                            abline( h = ax_ticks, col = self$style_info$grid$month$col, lty = self$style_info$grid$month$lty )

  }

  abline( h = 0, col = self$style_info$grid$zero$col, lty = self$style_info$grid$zero$lty )

  if( self$style_info$value$visible ) axis( 2, at = ax_ticks, labels = do.call( format, c( x = list( ax_ticks ), self$style_info$value$format ) ), las = 1, tick = FALSE )

} )

PlotDts$set( 'private', 'plot_time_axis_and_grid', function() {

  b = private$basis_limited

  switch(
    attr( b, 'type' ),
    POSIXct = {

      min10 = private$calc_intraday_marks( b, as.difftime( '00:10:00' ) )
      hours = private$calc_intraday_marks( b, as.difftime( '01:00:00' ) )

      min10[, xlab := format( t, ':%M'   ) ]
      hours[, xlab := format( t, '%H:%M' ) ]

    }
  )

  dates = b[, .( date, t = date, x = x_from ) ]
  month = b[, .( t = date[1], x = x_from[1] ), by = .( month = format( date, '%Y-%m' ) ) ]
  years = b[, .( t = date[1], x = x_from[1] ), by = .( year  = format( date, '%Y'    ) ) ]

  dates[, xlab := format( t, '%d'    ) ]
  month[, xlab := format( t, '%b'    ) ]
  years[, xlab := format( t, '%Y'    ) ]

  # plot grid
  x_grid = list(

    min10 = self$style_info$time$resolution %in% c( 'minute' ),
    hours = self$style_info$time$resolution %in% c( 'minute', 'hour' ),
    dates = self$style_info$time$resolution %in% c( 'minute', 'hour', 'day', 'month' ),
    month = self$style_info$time$resolution %in% c( 'minute', 'hour', 'day', 'month', 'year' ),
    years = self$style_info$time$resolution %in% c( 'minute', 'hour', 'day', 'month', 'year', 'years' )

  )

  self$x_grid = x_grid

  if( self$style_info$time$grid ) {

    if( x_grid$min10 ) abline( v = min10$x, lty = self$style_info$grid$minute$lty, col = self$style_info$grid$minute$col )
    if( x_grid$hours ) abline( v = hours$x, lty = self$style_info$grid$hour  $lty, col = self$style_info$grid$hour  $col )
    if( x_grid$dates ) abline( v = dates$x, lty = self$style_info$grid$day   $lty, col = self$style_info$grid$day   $col )
    if( x_grid$month ) abline( v = month$x, lty = self$style_info$grid$month $lty, col = self$style_info$grid$month $col )
    if( x_grid$years ) abline( v = years$x, lty = self$style_info$grid$year  $lty, col = self$style_info$grid$year  $col )

  }

  # plot axis
  x_labs = list(

    min10 = par( 'xaxt' ) != 'n' & self$style_info$time$resolution %in% c( 'minute' ),
    hours = par( 'xaxt' ) != 'n' & self$style_info$time$resolution %in% c( 'minute', 'hour' ),
    dates = par( 'xaxt' ) != 'n' & self$style_info$time$resolution %in% c( 'minute', 'hour', 'day'  ),
    month = par( 'xaxt' ) != 'n' & self$style_info$time$resolution %in% c( 'minute', 'hour', 'day', 'month', 'year' ),
    years = par( 'xaxt' ) != 'n' & self$style_info$time$resolution %in% c( 'minute', 'hour', 'day', 'month', 'year', 'years' )

  )

  if( self$style_info$time$visible ) {

    if( x_labs$hours ) hours[, axis( at = x, labels = xlab, side = 1, tick = FALSE, line = -1 ) ]
    if( x_labs$dates ) dates[, axis( at = x, labels = xlab, side = 1, tick = FALSE, line = -1 + x_labs$hours ) ]
    if( x_labs$month ) month[, axis( at = x, labels = xlab, side = 1, tick = FALSE, line = -1 + x_labs$hours + x_labs$dates ) ]
    if( x_labs$years ) years[, axis( at = x, labels = xlab, side = 1, tick = FALSE, line = -1 + x_labs$hours + x_labs$dates + x_labs$month ) ]

  }

} )

PlotDts$set( 'private', 'plot_candles', function( x, y, time_frame, col = 'auto', type = c( 'barchart', 'candlestick' ), gap = 0.2, border = NA ) {

  type = match.arg( type )

  if( col[[1]] == 'auto' ) {

    if( type == 'candlestick' ) {

      col = c( up = private$get_color(), self$style_info$candle$col[ c( 'flat', 'down' ) ] )

    } else {

      col = private$get_color()
    }

  }

  legend_info = data.table( col = col[[1]], pch = NA, lty = 1, lwd = 1 )

  o = y[[1]]
  h = y[[2]]
  l = y[[3]]
  c = y[[4]]

  if( missing( time_frame ) ) {

    time_frame = min( diff( x ), na.rm = T )

  }

  width = time_frame * ( 1 - gap ) / 2

  if( length( col ) > 1 ) col = ifelse( o < c, col$up, ifelse( o > c, col$down, col$flat ) )

  switch(
    type,
    barchart = {

      segments( x - width    , h, x - width, l, col = col )
      segments( x - width * 2, o, x - width, o, col = col )
      segments( x - width    , c, x        , c, col = col )

    },
    candlestick = {

      segments( x - width    , h, x - width, l, col = col )
      rect    ( x - width * 2, o, x        , c, col = col, border = border )

    }
  )

  legend_info


} )

PlotDts$set( 'private', 'plot_events', function( x, labels = NA, lty = 1, col = 'auto', lwd = 1 ) {

  if( col == 'auto' ) col = private$get_color()

  abline( v = x, lty = lty, col = col, lwd = lwd )
  axis( 3, at = x, tick = F, labels = labels, line = -1 )

  legend_info = data.table( lty, col, lwd )

} )

PlotDts$set( 'private', 'plot_lines', function( x, y, type = 'l', lty = 1, pch = NA, col = 'auto', bg = NA, lwd = 1, lend = 'round' ) {

  if( col == 'auto' ) col = private$get_color()

  args = as.list( environment() )
  do.call( lines, args )

  legend_info = data.table( lty, col, lwd, pch, bg, lend )

} )

PlotDts$set( 'private', 'plot_segments', function( x0, y0, x1 = x0, y1 = y0, col = 'auto', lty = 1, lwd = 1 ) {


  if( col[1] == 'auto' ) col = private$get_color()

  legend_info = data.table( col, lty, lwd )

  segments( x0, y0, x1, y1, col, lty, lwd )

  legend_info

} )

PlotDts$set( 'private', 'plot_histogram', function( x, y, gap = 0.2, col = 'auto', border = NA, time_frame = NULL, split = F, stacked = F, gap2 = 0.05, y0 = 0 ) {

  if( col[1] == 'auto' ) col = private$get_color( length( y ) )

  legend_info = data.table( col, pch = 15 )

  xright = x

  xleft = if( is.null( time_frame ) ) shift( x ) else x - time_frame

  xstep = min( xright - xleft, na.rm = T )

  xleft[1] = xright[1] - xstep

  xleft = xleft + xstep * gap

  ytop = y0
  ybottom = y0

  shift_step = split / length( y )

  shift = 0

  xw = xright - xleft

  mapply( function( y, col ) {

    if( stacked ) {

      ybottom <<- ytop

    }
    ytop <<- y

    if( split ) {

      shift <<- shift + shift_step
      rect( xleft + xw * ( shift - shift_step ) + xstep * gap2, ybottom, xleft + xw * shift, ytop, col = col, border = border )

    }
    else {

      rect( xleft, ybottom, xright, ytop, col = col, border = border )

    }

  }, y, col )

  legend_info

} )

PlotDts$set( 'private', 'plot_last_values', function() {

  x = private$last
  axis( 4, at = x, tick = F, labels = do.call( format, c( x = list( x ), self$style_info$value$format ) ), las = 1 )

} )

PlotDts$set( 'private', 'get_x', function( id, i = 1 ) {

  id.. = id
  private$t_to_x_map[ id == id.. & col_id == unique( col_id )[i], .( x ) ]

} )

PlotDts$set( 'private', 'get_visible', function( id ) {

  id.. = id
  private$t_to_x_map[ id == id.. & col_id == col_id[1], visible ]

} )

PlotDts$set( 'private', 'get_color', function( n = 1 ) {

  # init
  if( is.null( private$color_id ) ) {

    private$color_id = 0
    N = length( private$last ) * 1.5
    self$colors = if( N > length( distinct_colors ) ) colorRampPalette( distinct_colors )( N ) else distinct_colors

  }

  col = self$colors[ private$color_id + 1:n ]

  private$color_id = private$color_id + n

  if( any( is.na( col ) ) ) stop( 'no more automatic colors available, please set colors manually', call. = F )

  return( col )

} )

PlotDts$set( 'private', 'plot_legend', function() {

  private$legend_info[, {
    legend(
      legend    = label, col = col, pt.bg = bg, lty = lty, pch = pch, lwd = lwd,
      x         = self$style_info$legend$position,
      bg        = self$style_info$legend$col$background,
      box.col   = self$style_info$legend$col$frame,
      text.col  = self$style_info$legend$col$text,
      inset     = self$style_info$legend$inset,
      xpd       = TRUE,
      horiz     = self$style_info$legend$horizontal,
      y.intersp = 0.8
    )
  } ]

} )

PlotDts$set( 'private', 'do_task', function( task ) {

  switch(
    task$type,
    candles = {

      ds = private$data_summary[ match( task$names, name ) ]

      li = do.call(
        private$plot_candles, c(
          private$get_x( ds$id[1] ),
          list( y = private$data[[ ds$id[1] ]][, ds$name, with = F ] ),
          task$args
        ) )
      data.table( name = task$names[4], label = task$label, li )

    },
    lines = {

      ds = private$data_summary[ match( task$names, name ) ]

      args = as.data.table( task$args )

      li = ds[, do.call(
        private$plot_lines, c(
          private$get_x( id ),
          y = list( private$data[[ id ]][[ name ]] ),
          if( nrow( args ) == length( task$names ) ) args[ task$names == name ] else args
        ) ), by = .( id, col_id, name ) ]
      data.table( label = task$labels, li )

    },
    events = {

      ds = private$data_summary[ match( task$name, name ) ]

      li = do.call(
        private$plot_events, c(
          private$get_x( ds$id ),
          task$args
        ) )
      data.table( name = task$name, label = task$label, li )

    },
    histogram = {

      ds = private$data_summary[ match( task$names, name ) ]
      if( uniqueN( ds$id ) > 1 ) stop( 'histogram data must be from single data set', call. = F )

      li = do.call(
        private$plot_histogram, c(
          private$get_x( ds$id[1] ),
          list( y = private$data[[ ds$id[1] ]][, ds$name, with = F ] ),
          task$args
        ) )
      data.table( name = task$names, label = task$labels, li )

    },
    segments = {

      ds = private$data_summary[ match( task$names, name ) ]

      if( length( task$names ) != 2 ) stop( 'segments data must combine two names', call. = F )
      if( uniqueN( ds$id ) > 1 ) stop( 'segments data must be from single data set', call. = F )

      li = do.call(
        private$plot_segments, c(
          list( x0 = private$get_x( ds$id[1], 1 )$x ),
          list( x1 = private$get_x( ds$id[1], 2 )$x ),
          list( y0 = private$data[[ ds$id[1] ]][[ ds$col_id[1] ]] ),
          list( y1 = private$data[[ ds$id[1] ]][[ ds$col_id[2] ]] ),
          task$args
        ) )
      data.table( name = task$names[1], label = task$label, li )

    }

  )

} )

PlotDts$set( 'private', 'add_task', function( type, args ) {

  i = length( private$tasks ) + 1
  private$tasks[[ i ]] = c( type = type, args )

} )

PlotDts$set( 'public', 'plot', function() {

  if( is.null( private$basis_limited ) ) private$set_limits()
  private$plot_basis()
  legend_info_mask = data.table( label = '', last = 0., pch = 1, col = '', lty = 1, lwd = 1, bg = '' )[0]
  private$legend_info = rbindlist( c( list( legend_info_mask ), lapply( private$tasks, private$do_task ) ), fill = T )

  private$legend_info[ private$data_summary, last := i.last, on = 'name' ]
  setorder( private$legend_info, -last )

  if( self$style_info$value$last     ) private$plot_last_values()
  if( self$style_info$legend$visible ) private$plot_legend()

  invisible( self )

} )

PlotDts$set( 'public', 'lines', function( names = 'auto', labels = names, ... ) {

  if( all( names == 'auto' ) )  {

    names = private$data_summary[ class %in% c( 'numeric', 'double' ), name ]
    labels = names

  }

  private$add_task( type = 'lines', c( as.list( environment() ), list( args = list( ... ) ) ) )
  invisible( self )

} )

PlotDts$set( 'public', 'candles', function( names = c( 'open', 'high', 'low', 'close' ), label = 'candles', ... ) {

  private$add_task( type = 'candles', c( as.list( environment() ), list( args = list( ... ) ) ) )
  invisible( self )

} )

PlotDts$set( 'public', 'events', function( name, label = name, ... ) {

  private$add_task( type = 'events', c( as.list( environment() ), list( args = list( ... ) ) ) )
  invisible( self )

} )

PlotDts$set( 'public', 'histogram', function( names, labels = names, y0 = 0, ... ) {

  self$ylim = c( self$ylim, y0 )

  private$add_task( type = 'histogram', c( as.list( environment() ), list( args = list( ... ) ) ) )
  invisible( self )

} )

PlotDts$set( 'public', 'segments', function( names, label = 'segments', ... ) {

  private$add_task( type = 'segments', c( as.list( environment() ), list( args = list( ... ) ) ) )
  invisible( self )

} )
