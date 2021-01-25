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

#' Convert time zone to 'UTC' without changing value
#'
#' @param x \code{POSIXct} vector
#'
#' @examples
#' \donttest{
#'
#' Sys.time()
#' to_UTC( Sys.time() )
#'
#' }
#'
#' @export
to_UTC = function( x ) {

  if( c( attr( x, 'tzone' ), '' )[1] == 'UTC' ) return( x )
  lubridate::force_tz( x, 'UTC', roll = T )

}
