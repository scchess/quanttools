# Copyright (C) 2016-2018 Stanislav Kovalevsky
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

#' @title Vectorized switch
#' @param x old values
#' @param mapping named vector mapping \code{c( old_names = new_names )}
#' @param default default value if no mapping found, if set to NULL original value kept
#' @export
vswitch = function( x, mapping, default = NA ) {

  # https://stackoverflow.com/questions/31156957/use-of-switch-in-r-to-replace-vector-values

  xx = mapping[ x ]
  if( is.null( default ) ) {

    xx[ is.na( xx ) ] = x[ is.na( xx ) ]

  } else {

    xx[ is.na( xx ) ] = default

  }
  xx
}
