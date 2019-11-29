# Copyright (C) 2016 Stanislav Kovalevsky
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

#' Download historical market data
#'
#' @param symbol symbol name
#' @param from,to text dates in format \code{"YYYY-mm-dd"}
#' @param period candle period \code{tick, 1min, 5min, 10min, 15min, 30min, hour, day, week, month}
#' @param split.adjusted should data be split adjusted?
#' @param dividend.adjusted should data be split adjusted?
#' @param local should data be loaded from local storage? See 'Local Storage' section
#' @param code futures or option code name, e.g. \code{"RIU6"}
#' @param contract,frequency,day_exp same as in \code{\link{gen_futures_codes}}
#' @name get_market_data
#' @details Use external websites to get desired symbol name for
#' \href{https://www.finam.ru/profile/moex-akcii/sberbank/export/}{Finam},
#' \href{https://www.moex.com/en/derivatives/contracts.aspx}{MOEX},
#' \href{https://www.iqfeed.net/symbolguide/index.cfm?symbolguide=lookup}{IQFeed},
#' \href{https://finance.yahoo.com/}{Yahoo} \cr

#' \subsection{IQFeed}{
#'
#'  \code{data.table} with following data returned:
#'  \tabular{ll}{
#'             daily:    \tab date, open, high, low, close, volume, open_interest  \cr
#'             intraday: \tab date, open, high, low, close, volume  \cr
#'             tick:     \tab time, price, volume, size, bid, ask, tick_id, basis_for_last, trade_market_center, trade_conditions  \cr
#'  }
#'  See \link{iqfeed} specification for details. \cr
#'  Note: \code{from} and \code{to} can be set as text in format \code{"YYYY-mm-dd HH:MM:SS"}.
#'
#'  }
#'  \subsection{Finam}{
#'
#'  \code{data.table} with following data returned:
#'  \tabular{ll}{
#'             daily:    \tab date, open, high, low, close, volume  \cr
#'             intraday: \tab date, open, high, low, close, volume  \cr
#'             tick:     \tab time, price, volume                   \cr
#'  }
#'
#'  }
#'  \subsection{Yahoo}{
#'
#'  \code{data.table} with following data returned:
#'  \tabular{ll}{
#'             daily:    \tab date, open, high, low, close, adj_close, volume  \cr
#'             splits and dividends:    \tab date, value, event  \cr
#'  }
#'
#'  }
#'  \subsection{MOEX}{
#'    data can be retrieved from local storage only in order to minimize load on MOEX data servers. See 'Local Storage' section.
#'  }
#'
#' @note
#' Timestamps timezones set to UTC. \cr
#' @section Local Storage: It is recommended to store tick market data locally.
#' Load time is reduced dramatically. It is a good way to collect market data as
#' e.g. IQFeed gives only 180 days of tick data if you would need more it will
#' cost you a lot. See \code{\link{store_market_data}} for details. \cr
#' Only IQFeed, Finam and MOEX data supported.
#'
#' @examples
#' \donttest{
#' get_finam_data( 'GAZP', '2015-01-01', '2016-01-01' )
#' get_finam_data( 'GAZP', '2015-01-01', '2016-01-01', 'hour' )
#' get_finam_data( 'GAZP', Sys.Date(), Sys.Date(), 'tick' )
#'
#' get_iqfeed_data( 'MSFT', '2015-01-01', '2016-01-01' )
#' get_iqfeed_data( 'MSFT', '2015-01-01', '2016-01-01', 'hour' )
#' get_iqfeed_data( 'MSFT', Sys.Date() - 3, Sys.Date() , 'tick' )
#'
#' get_yahoo_data( 'MSFT', '2015-01-01', '2016-01-01' )
#'
#' get_moex_futures_data( 'RIH9', '2009-01-01', '2009-02-01', 'tick', local = T )
#' get_moex_options_data( 'RI55000C9', '2009-01-01', '2009-02-01', 'tick', local = T )
#' get_moex_continuous_futures_data( 'RI', '2016-01-01', '2016-11-01', frequency = 3, day_exp = 15 )
#'
#' }
NULL


# download market data from Finam server
#' @rdname get_market_data
#' @export
get_finam_data = function( symbol, from, to = from, period = 'day', local = FALSE ) {

  if( local ) {

    storage = DataStorage$new( .settings$finam_storage, label = 'finam' )
    return( storage$get( symbol, from, to, period ) )

  }

  finam_download_data( symbol, from, to, period, trial_limit = .settings$finam_retry_limit, trial_sleep = .settings$finam_retry_sleep, verbose = .settings$finam_verbose )

}
#' @rdname get_market_data
#' @export
get_iqfeed_data = function( symbol, from, to = from, period = 'day', local = FALSE ) {

  from = format( from )
  to   = format( to   )

  if( nchar( from ) == 10 && nchar( to ) > 10 ) from = paste( from, '00:00:00' )

  if( local ){

    storage = DataStorage$new( .settings$iqfeed_storage, label = 'iqfeed' )
    return( storage$get( symbol, from, to, period ) )

  }

  # limits and restrictions
  # http://www.iqfeed.net/index.cfm?displayaction=data&section=services
  switch(
    period,
    'tick' = {

      now = Sys.time()
      attr( now, 'tzone' ) = 'America/New_York'

      is_trading_hours         = format( now, '%H:%M' ) %bw% c( '09:30', '16:00' )
      is_over_7_days_requested = ( Sys.Date() - as.Date( from ) ) > as.difftime( 7, units = 'days' )
      is_weekend               = format( now, '%u' ) %in% 6:7

      if( is_trading_hours && is_over_7_days_requested && !is_weekend ) {

        stop( 'please download tick data older than 8 calendar days outside trading hours [ 9:30 - 16:00 America/New York ] or during weekend', call. = FALSE )

      }

      is_over_180_days_requested = as.Date( now ) - as.Date( from ) > as.difftime( 180, units = 'days' )

      if( is_over_180_days_requested ) {

        from = format( as.Date( now ) - as.difftime( 180, units = 'days' ) )
        warning( 'tick data older than 180 calendar days not available', call. = FALSE )

      }

    },
    '1min' = {

      if( as.Date( from ) < as.Date( '2005-02-01' ) ) {

        from = '2005-02-01'
        warning( '1min data before \'2005-02-01\' not available', call. = FALSE )

      }

    }

  )
  if( as.Date( from ) > as.Date( to ) ) return( NULL )

  interval = switch( period, '1min' = 1, '5min' = 5, '10min' = 10, '15min' = 15, '30min' = 30, 'hour' = 60, 0 ) * 60

  data = switch(
    period,
    'tick'  = .get_iqfeed_ticks( symbol, from, to ),
    'day'   = .get_iqfeed_daily_candles( symbol, from, to ),
    .get_iqfeed_candles( symbol, from, to, interval = interval )
  )

  if( !is.null( data ) && nrow( data ) == 0 ) data = NULL
  return( data )

}
#' @rdname get_market_data
#' @export
get_moex_options_data = function( code, from, to = from, period = 'tick', local = TRUE ) {

  .get_moex_data( code = code, from = from, to = to, period = period, local = local, type = 'options' )

}
#' @rdname get_market_data
#' @export
get_moex_futures_data = function( code, from, to = from, period = 'tick', local = TRUE ) {

  .get_moex_data( code = code, from = from, to = to, period = period, local = local, type = 'futures' )

}
#' @rdname get_market_data
#' @export
get_moex_continuous_futures_data = function( contract, from, to, frequency, day_exp ) {

  schedule = gen_futures_codes( contract, from, to, frequency, day_exp, year_last_digit = T )

  trades = schedule[, get_moex_futures_data( code, from, to ), by = contract_id ][]
  setcolorder( trades, c( 'time', 'price', 'volume', 'id', 'contract_id' )  )
  code = contract_id = NULL
  trades[, code := schedule$code[ contract_id ] ][]
  gc()
  return( trades )

}

.get_moex_data = function( code, from, to = from, period = 'tick', local = TRUE, type = c( 'options', 'futures' ) ) {

  if( period != 'tick' ) stop( 'only \'tick\' data supported' )
  if( !local ) stop( 'only \'local = TRUE\' flag supported' )
  type = match.arg( type )

  dir_data = paste0( .settings$moex_storage, '/', type, '/' )

  files = list.files( dir_data, pattern = '.rds', full.names = T )
  dates = gsub( '.*/|\\..*', '', files )

  data = vector( length( dates[ from <= dates & dates <= to ] ), mode = 'list' )
  i = 1

  for( file in files[ from <= dates & dates <= to ] ) {

    code_ = code

    dat_time = price = amount = NULL

    Nosystem = 0
    data[[ i ]] = readRDS( file )[ code == code_ & Nosystem != 1, list( time = dat_time, price, volume = as.integer( amount ), id = 1:.N ) ]

    i = i + 1

  }

  data = rbindlist( data, use.names = T, fill = T )[]
  gc()
  return( data )

}
