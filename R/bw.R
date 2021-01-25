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

#' Check if values are between specified interval
#'
#' @param x vector
#' @param interval vector of length 1 or 2, see 'Examples' section
#' @details If second element of interval contains time selection is closed on the left only (\code{a <= x < b}) otherwise selection is closed (\code{a <= x <= b}).
#' @examples
#' \donttest{
#'
#' data( ticks )
#'
#' # bw is very usefull to filter time series data:
#' # select single year
#' ticks[ time %bw% '2016' ]
#'
#' # select single month
#' ticks[ time %bw% '2016-05' ]
#'
#' # select single date
#' ticks[ time %bw% '2016-05-11' ]
#' # also works with Date class
#' ticks[ time %bw% as.Date( '2016-05-11' ) ]
#'
#' # select single hour
#' ticks[ time %bw% '2016-05-11 10' ]
#'
#' # select single minute
#' ticks[ time %bw% '2016-05-11 10:20' ]
#'
#' # select single second
#' ticks[ time %bw% '2016-05-11 10:20:53' ]
#'
#' # select between two months inclusive
#' ticks[ time %bw% '2016-05/2016-08' ]
#'
#' # select from month begin and date
#' ticks[ time %bw% '2016-05/2016-06-23' ]
#'
#' # select between two timestamps
#' ticks[ time %bw% '2016-05-02 09:30/2016-05-02 11:00' ]
#' # also works with incomplete timestamps
#' ticks[ time %bw% '2016-05-02 09:30/2016-05-02 11' ]
#'
#' # select all dates but with time between 09:30 and 16:00
#' ticks[ time %bw% '09:30/16:00' ]
#'
#' # also bw can be used as a shortcut for 'a <= x & x <= b' for non-'POSIXct' classes:
#' # numeric
#' 15:25 %bw% c( 10, 20 )
#'
#' # character
#' letters %bw% c( 'a', 'f' )
#'
#' # dates
#' Sys.Date() %bw% ( Sys.Date() + c( -10, 10 ) )
#'
#'
#' }
#' @name bw
#' @export
bw = function( x, interval ) {

  if( is.null( interval ) ) return( !vector( length = length( x ) ) )
  if( is.character( interval ) ) {

    if( inherits( x, 'POSIXct' ) ) {

      tz = c( attr( x, 'tzone' ), '' )[1]
      interval = .text_to_time_interval( interval, tz )
      if( is.numeric( interval ) ) {
        return( round( as.numeric( to_UTC( x ) ) %% ( 24 * 60 * 60 ), 6 ) %bw% interval )
      }
      return( interval[1] <= x & x < interval[2] )

    }
    if( inherits( x, 'Date' ) ) {

      interval = as.Date( .text_to_time_interval( interval, 'UTC' ) )
      return( interval[1] <= x & x < interval[2] )

    }

  }
  if( length( interval ) == 1 ) {

    if( inherits( interval, 'Date' ) ) {

      interval[2] = interval + 1

      if( inherits( x, 'POSIXct' ) ) {

        interval = fasttime::fastPOSIXct( interval, attr( x, 'tzone' ) )

      }
      return( interval[1] <= x & x < interval[2] )

    }
    stop( 'interval must contain two elements' )

  }

  x >= interval[1] & x <= interval[2]

}

#' @name bw
#' @export
`%bw%` = bw


.text_to_time_interval = function( x, tz = '' ) {

  text_to_timestamp_interval = function( x, tz = '', date = '' ) {

    # x = '2020'
    # tz = 'UTC'

    x = unlist( stringi::stri_split_fixed( x, '/' ) )

    if( length( x ) == 2 ) {

      interval_1 = text_to_timestamp_interval( x[1], tz )
      interval_2 = text_to_timestamp_interval( x[2], tz, interval_1$date )

      ret = list(
        interval = c( interval_1$interval[1], interval_2$interval[2] ),
        date = interval_1$date
      )

      return( ret )

    }

    info = c( stringi::stri_length( x ), stringi::stri_count( x, fixed = c( '-', ':' ) ) )

    case = paste( info, collapse = '-' )

    switch(
      case,
      '2-0-0' = , # HH
      '4-0-1' = , # HH:M
      '5-0-1' = , # HH:MM
      '7-0-2' = , # HH:MM:S
      '8-0-2' =   # HH:MM:SS
        {

          if( date == '' ) {

            interval = text_to_timestamp_interval( paste( '1970-01-01', x ), tz )
            interval$interval = interval$interval - as.POSIXct( '1970-01-01', tz )
            interval$date = ''

          } else {

            interval = text_to_timestamp_interval( paste( date, x ), tz )

          }

          return( interval )

        }
    )

    start_text = stringi::stri_sub_replace( '1970-01-01 00:00:00', 1, info[1], replacement = x )

    start_time = lubridate::fast_strptime( start_text, '%Y-%m-%d %H:%M:%S', tz = tz, lt = T )
    end_time   = start_time

    switch(
      case,
      '4-0-0'  = { end_time$mon  = end_time$mon  + 12 }, # YYYY
      '7-1-0'  = { end_time$mon  = end_time$mon  + 01 }, # YYYY-mm
      '10-2-0' = { end_time$mday = end_time$mday + 01 }, # YYYY-mm-dd
      '13-2-0' = { end_time$hour = end_time$hour + 01 }, # YYYY-mm-dd HH
      '15-2-1' = { end_time$min  = end_time$min  + 10 }, # YYYY-mm-dd HH:M
      '16-2-1' = { end_time$min  = end_time$min  + 01 }, # YYYY-mm-dd HH:MM
      '18-2-2' = { end_time$sec  = end_time$sec  + 10 }, # YYYY-mm-dd HH:MM:S
      '19-2-2' = { end_time$sec  = end_time$sec  + 01 }, # YYYY-mm-dd HH:MM:SS
      stop( 'usupported interval format' )
    )

    interval = as.POSIXct( c( start_time, end_time ) )
    attr( interval, 'tzone' ) = tz

    list(
      interval = interval,
      date = substr( start_text, 1, 10 )
    )

  }

  interval = text_to_timestamp_interval( x, tz )$interval

  if( inherits( interval, 'difftime' )  ) {

    interval = as.numeric( interval, units = 'secs' )

  }

  interval

}

