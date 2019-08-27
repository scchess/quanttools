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

#' Store historical market data
#'
#' @param from,to text dates in format \code{"YYYY-mm-dd"}
#' @param verbose show progress?
#' @name store_market_data
#' @details
#' See example below.
#'
#' @examples
#' \donttest{
#'
#' ## Finam data storage
#' settings = list(
#'   # set storage path, it is perfect to use Solid State Drive for data storage
#'   # it is no problem to move storage folder just don't forget to set new path in settings
#'   finam_storage = paste( path.expand('~') , 'Market Data', 'finam', sep = '/' ),
#'   # add some symbols
#'   finam_symbols = c( 'GAZP', 'SBER' ),
#'   # and set storage start date
#'   finam_storage_from = '2016-09-01'
#' )
#' QuantTools_settings( settings )
#' # now it is time to add some data into storage. You have three options here:
#'
#'   # 1 update storage with data from last date available until today
#'   # it is very convenient to create a script with this function and
#'   # run it every time you need to update your storage
#'   store_finam_data()
#'
#'   # 2 update storage with data from last date available until specified date
#'   store_finam_data( to = '2016-09-28' )
#'
#'   # 3 update storage with data between from and to dates,
#'   # if data already present it will be overwritten
#'   store_finam_data( from = '2016-01-01', to = '2016-01-10' )
#'
#' # set local = TRUE to load from just created local market data storage
#' get_finam_data( 'GAZP', '2016-09-01', '2016-09-28', 'tick', local = T )
#'
#' ## IQFeed data storage
#' settings = list(
#'   # set storage path, it is perfect to use Solid State Drive for data storage
#'   # it is no problem to move storage folder just don't forget to set new path in settings
#'   iqfeed_storage = paste( path.expand('~') , 'Market Data', 'iqfeed', sep = '/' ),
#'   # add some symbols
#'   iqfeed_symbols = c( 'AAPL', '@ES#' ),
#'   # and set storage start date
#'   iqfeed_storage_from = format( Sys.Date() - 3 )
#' )
#' QuantTools_settings( settings )
#' # now it is time to add some data into storage. You have three options here:
#'
#'   # 1 update storage with data from last date available until today
#'   # it is very convenient to create a script with this function and
#'   # run it every time you need to update your storage
#'   store_iqfeed_data()
#'
#'   # 2 update storage with data from last date available until specified date
#'   store_iqfeed_data( to = format( Sys.Date() ) )
#'
#'   # 3 update storage with data between from and to dates,
#'   # if data already present it will be overwritten
#'   store_iqfeed_data( from = format( Sys.Date() - 3 ), to = format( Sys.Date() ) )
#'
#' # set local = TRUE to load from just created local market data storage
#' get_iqfeed_data( 'AAPL', format( Sys.Date() - 3 ), format( Sys.Date() ), 'tick', local = T )
#'
#' ## MOEX data storage
#' settings = list(
#'   # set MOEX data url
#'   moex_data_url = 'url/to/moex/data',
#'   # set storage path, it is perfect to use Solid State Drive for data storage
#'   # it is no problem to move storage folder just don't forget to set new path in settings
#'   moex_storage = paste( path.expand('~') , 'Market Data', 'moex', sep = '/' ),
#'   # and set storage start date
#'   moex_storage_from = '2003-01-01'
#' )
#' QuantTools_settings( settings )
#' # now it is time to add some data into storage. You have three options here:
#'
#'   # 1 update storage with data from last date available until today
#'   # it is very convenient to create a script with this function and
#'   # run it every time you need to update your storage
#'   store_moex_data()
#'
#'   # 2 update storage with data from last date available until specified date
#'   store_moex_data( to = format( Sys.Date() ) )
#'
#'   # 3 update storage with data between from and to dates,
#'   # if data already present it will be overwritten
#'   store_moex_data( from = format( Sys.Date() - 3 ), to = format( Sys.Date() ) )
#'
#' # set local = TRUE to load from just created local market data storage
#' get_moex_futures_data( 'RIH9', '2009-01-01', '2009-02-01', 'tick', local = T )
#'
#' }
#' @rdname store_market_data
#' @export
store_finam_data = function( from = NULL, to = NULL, verbose = TRUE ) {

  symbols = .settings$finam_symbols

  if( is.null( symbols ) ) stop( 'please set symbols vector via QuantTools_settings( \'finam_symbols\', c( \'symbol_1\', ...,\'symbol_n\' ) ) ' )

  for( symbol in symbols ) .store_finam_data( symbol, '1min', from, to, split = 'month' )

  for( symbol in symbols ) .store_finam_data( symbol, 'tick', from, to, split = 'day' )

}

.store_finam_data = function( symbol, period = 'tick', from = NULL, to = NULL, split = 'none' ) {

  valid_period = c( 'tick', '1min' )
  if( !period %in% valid_period ) stop( period, ' is invalid period, valid periods are ', paste0( valid_period, collapse = ', ' ) )

  getter = function( symbol, from, to, period ) {

    get_finam_data( symbol, from, to, period = period )

  }

  self = DataStorage$new( .settings$finam_storage, .settings$finam_storage_from, getter, label = 'finam' )
  self$store( symbol, period, from, to, split )

}

#' @rdname store_market_data
#' @export
# iqfeed
store_iqfeed_data = function( from = NULL, to = NULL, verbose = TRUE ) {

  symbols = .settings$iqfeed_symbols

  if( is.null( symbols ) ) stop( 'please set symbols vector via QuantTools_settings( \'iqfeed_symbols\', c( \'symbol_1\', ...,\'symbol_n\' ) ) ' )

  for( symbol in symbols ) {

    .store_iqfeed_data( symbol, '1min', from, to, split = 'month' )
    .store_iqfeed_data( symbol, 'tick', from, to, split = 'day'   )

  }

}

.store_iqfeed_data = function( symbol, period = 'tick', from = NULL, to = NULL, split = 'none' ) {

  valid_period = c( 'tick', '1min' )
  if( !period %in% valid_period ) stop( period, ' is invalid period, valid periods are ', paste0( valid_period, collapse = ', ' ) )

  getter = function( symbol, from, to, period ) {

    get_iqfeed_data( symbol, from, to, period = period )

  }

  self = DataStorage$new( .settings$iqfeed_storage, .settings$iqfeed_storage_from, getter, label = 'iqfeed' )
  self$store( symbol, period, from, to, split = split )

}

## ---- deprecated ----
# moex
deprecated_store_moex_data = function( from = NULL, to = format( Sys.Date() ), verbose = TRUE ) {

  save_dir = .settings$moex_storage
  if( save_dir == '' ) stop( 'please set storage path via QuantTools_settings( \'moex_storage\', \'/storage/path/\' ) ' )
  temp_dir = .settings$temp_directory
  if( temp_dir == '' ) stop( 'please set temp directory path via QuantTools_settings( \'temp_directory_\', \'/temp/directory/path/\' ) ' )

  from_is_null = is.null( from )
  if( from_is_null ) from = NULL

  dates_available = gsub( '.rds', '', list.files( paste0( save_dir, '/futures/' ), pattern = '.rds' ) )
  if( is.null( from ) && length( dates_available ) == 0 ) {

    from = .settings$moex_storage_from
    if( from == '' ) stop( 'please set moex storage start date via QuantTools_settings( \'moex_storage_from\', \'YYYYMMDD\' )' )
    message( 'no data found in storage, \ntrying to download since storage start date' )

  }
  if( is.null( from ) && to >= max( dates_available ) ) {

    from = max( dates_available )
    message( paste( 'dates to be added:', from, '-', to ) )

  }

  for( date in as.Date( from ):as.Date( to ) ) {

    date = as.Date( date, origin = '1970-01-01' )

    dir_fut = paste0( save_dir, '/futures' )
    dir_opt = paste0( save_dir, '/options' )
    dir.create( dir_fut, recursive = T, showWarnings = F )
    dir.create( dir_opt, recursive = T, showWarnings = F )

    file_fut = paste0( dir_fut, '/', format( date ), '.rds' )
    file_opt = paste0( dir_opt, '/', format( date ), '.rds' )

    year = format( date, '%Y' )
    yymmdd = format( date, '%y%m%d')


    data_url = .settings$moex_data_url

    url_exists = function( url ) TRUE # !RCurl::url.exists

    if( !url_exists( data_url ) ) stop( 'please set MOEX data url via QuantTools_settings( \'moex_data_url\', \'/moe/data/url/\' )' )

    url = paste0( data_url, '/', year, '/FT', yymmdd, '.ZIP')

    if( !url_exists( url ) ) next
    file_zip = paste0( temp_dir, '/', yymmdd, '.zip' )


    unlink( list.files( temp_dir, full.names = T ), force = T, recursive = T )
    dir.create( temp_dir, recursive = T, showWarnings = F )

    download.file( url, destfile = file_zip, mode = 'wb', quiet = T )

    unzip( file_zip, exdir = temp_dir )
    #cmd = paste0( '7z x "', file_zip, '" -o"', temp_dir, '"' )
    #shell( cmd, wait = T )

    files = list.files( temp_dir, pattern = 'ft|ot|FT|OT', recursive = T, full.names = T )

    ft = files[ grepl( 'ft', tolower( files ) ) ]
    ot = files[ grepl( 'ot', tolower( files ) ) ]

    is_xls = grepl( '.xls', tolower( ft ) )

    format_trades = function( trades ) {

      code = contract = dat_time = NULL
      trades[, code := as.factor( code ) ]
      trades[, contract := as.factor( contract ) ]
      trades[, dat_time := fasttime::fastPOSIXct( dat_time, 'UTC' ) ]

    }

    if( is_xls ) {

      . = capture.output( { sheets = readxl::excel_sheets( ft ) } )
      fut_sheet = sheets[ grepl( 'fut.*trade', sheets ) ]
      opt_sheet = sheets[ grepl( 'opt.*trade', sheets ) ]

      if( is.na( fut_sheet ) ) {

        message( 'no futures trades sheet available' )

      } else {

        . = capture.output( { trades = setDT( readxl::read_excel( ft, sheet = fut_sheet ) ) } )

        format_trades( trades )
        saveRDS( trades, file_fut )

      }

      if( is.na( opt_sheet ) ) {

        message( 'no options trades sheet available' )

      } else {

        . = capture.output( { trades = setDT( readxl::read_excel( ft, sheet = opt_sheet ) ) } )

        format_trades( trades )
        saveRDS( trades, file_opt )

      }

      if( verbose ) message( date, ' saved' )

    } else {

      if( is.null( ft ) ) {

        message( 'no futures trades file available' )

      } else {

        trades = fread( ft )

        format_trades( trades )
        saveRDS( trades, file_fut )

      }

      if( is.null( ot ) ) {

        message( 'no options trades file available' )

      } else {

        if( date == as.Date( '2008-09-15' ) ) next ## embeded nul

        trades = fread( ot )

        format_trades( trades )
        saveRDS( trades, file = file_opt )

      }

      if( verbose ) message( date, ' saved' )

    }

  }

}
