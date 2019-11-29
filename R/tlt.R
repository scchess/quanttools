#' @title Lapply function to data.table by row.
#' @description T.ranspose L.apply T.ranspose algorithm used. First step is to transpose initial data.table so rows become columns. Second is to lapply function to new columns. And the last step is to transpose back to original shape.
#' @param x data.table
#' @param fun function
#' @export
tlt = function( x, fun, ... ) {

  res = transpose( lapply( transpose( x ), fun, ... ) )
  setDT( res )

  if( length( res ) == length( x ) ) setnames( res, names( x ) )

  res[]

}
