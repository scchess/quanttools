# Copyright (C) 2018 Stanislav Kovalevsky
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

# Market Data Storage Class
#
# Currently supports storage of at most two data resolutions: tick and non-tick.
# Tick data stored using daily files and non-tick data stored using monthly files.
# Retrieves stored data for periods:  'tick', '1min', '5min', '10min', '15min', '30min', 'hour', 'day'.

DataStorage = R6::R6Class( 'DataStorage', lock_objects = F )
DataStorage$set( 'public', 'initialize', function( path, start = NULL, getter = NULL, label = '' ) {

  if( path  == '' ) stop( 'please set storage path via QuantTools_settings( \'', label, '_storage\', \'/storage/path/\' ) ', call. = FALSE )
  if( !is.null( start ) && start == '' ) stop( 'please set ', label, ' storage start date via QuantTools_settings( \'', label, '_storage_from\', \'YYYYMMDD\' )', call. = FALSE )

  if( !is.null( getter ) && !identical( formalArgs( getter ), c( 'symbol', 'from', 'period' ) ) ) stop( 'getter must have \'symbol\', \'from\' and \'period\' arguments' )

  self$path   = path
  self$start  = start
  self$getter = getter
  self$label  = label

} )
DataStorage$set( 'public', 'store', function( symbol, period ) {

  message( 'store ', symbol, ' ', period, ' ', self$label, ' data...' )

  pattern          = switch( period, tick = '\\d{4}-\\d{2}-\\d{2}.rds', '\\d{4}-\\d{2}.rds' )
  file_name_format = switch( period, tick = '%Y-%m-%d'                , '%Y-%m'             )

  save_dir = paste0( self$path, '/', symbol, '/' )
  if( !dir.exists( save_dir ) ) dir.create( save_dir, recursive = T )

  files = list.files( save_dir, pattern, full.names = T )

  no_historical_data_avaliable = length( files ) == 0

  if( no_historical_data_avaliable ) {

    from = self$start

    message( 'not found in storage, \ntrying to download since storage start date: ', from )

    data = self$getter( symbol, from, period )

    if( is.null( data ) ) { message( 'no data available!' ); return( Sys.time() ) }

  } else {

    file_recent = max( files )

    data_recent = readRDS( file_recent )

    from = data_recent[ max( 1, .N ), time ]

    to = Sys.time()
    attr( to, 'tzone' ) = attr( from, 'tzone' )

    message( paste( 'data to be added:', from, '-', to, 'UTC' ) )

    data = self$getter( symbol, from, period )
    if( is.null( data ) ) { message( 'no data available!' ); return( Sys.time() ) }

    data = rbind( data_recent[ time < data[ 1, time ] ], data )

  }

  data[, file := format( time, file_name_format ) ]

  data[ , {

    saveRDS( .SD, paste0( save_dir, file, '.rds' ) )
    message( paste( file, 'added' ) )

  }, by = file ]

  rm( data ); gc()

  message( 'done!' )

  return( Sys.time() )

} )
DataStorage$set( 'public', 'get', function( symbol, from, to, period ) {

  pattern          = switch( period, tick = '\\d{4}-\\d{2}-\\d{2}.rds', '\\d{4}-\\d{2}.rds' )
  file_name_format = switch( period, tick = '%Y-%m-%d'                , '%Y-%m'             )

  save_dir = paste0( self$path, '/', symbol, '/' )
  if( !dir.exists( save_dir ) ) {

    warning( 'no historical ', self$label, ' ', period, ' data available for ', symbol, call. = FALSE )

    return( NULL )

  }

  files = list.files( save_dir, pattern, full.names = T )

  if( any( nchar( c( from, to ) ) > 10 ) ) {

    if( nchar( from ) > 10 & nchar( to ) <= 10 ) stop( 'to must also have time resolution', call. = FALSE )

    from  = as.POSIXct( from, tz = 'UTC' ) + 1e-6
    to    = as.POSIXct( to  , tz = 'UTC' )

  }
  if( from > to ) stop( "'to' must be greater than 'from'", call. = FALSE )

  time_range = as.Date( c( from, to ) )

  file_range = format( time_range, file_name_format )

  file_periods = gsub( '.rds', '', regmatches( files, regexpr( pattern, files ) ) )

  files = files[ file_periods %bw% file_range ]

  no_historical_data_avaliable = length( files ) == 0

  if( no_historical_data_avaliable ) {

    data = NULL

  } else {

    data = data.table( file = files )[, readRDS( file ), by = file ][ time %bw% c( from, to ), -'file' ]

  }
  if( is.null( data ) || nrow( data ) == 0 ) return( NULL )

  if( period != 'tick' ) {

    switch(
      period,
      '1min'  = { n =  1; units = 'mins' },
      '5min'  = { n =  5; units = 'mins' },
      '10min' = { n = 10; units = 'mins' },
      '15min' = { n = 15; units = 'mins' },
      '30min' = { n = 30; units = 'mins' },
      'hour'  = { n =  1; units = 'hour' },
      'day'   = { n =  1; units = 'days' }
    )

    open = high = low = close = volume = NULL
    data = data[ , list(
      open   = open[1],
      high   = max( high ),
      low    = min( low ),
      close  = close[.N],
      volume = sum( volume )
    ), by = list(
      time = ceiling_POSIXct( time, n, units )
    ) ]

    if( period == 'day' ) {

      data[, time := as.Date( time ) - 1 ]
      setnames( data, 'time', 'date' )

    }

  }

  data[]

} )
