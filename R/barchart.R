barchart_connect = function( force = F ) {

  if( !force ) if( !is.null( barchart.globals$session ) ) {

    if( barchart.globals$expiration - Sys.time() > 0 ) return( F )

  }

  message( 'barchart connecting...' )
  header_useragent = httr::add_headers( 'user-agent' = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:86.0) Gecko/20100101 Firefox/86.0' )
  barchart.globals$session = rvest::session( 'https://www.barchart.com/stocks/quotes/AAPL/interactive-chart', header_useragent )
  cookies = as.data.table( barchart.globals$session$response$cookies )
  barchart.globals$header_token = httr::add_headers( 'x-xsrf-token' = URLdecode( cookies[ name == 'XSRF-TOKEN', value ] ) )
  barchart.globals$expiration = cookies[ name == 'XSRF-TOKEN', expiration ]
  message( 'barchart connected' )
  return( T )

}
#' @export
barchart_get_intraday = function( symbol, from, to, tzone, interval, extended = F ) {

  # https://help.barchart.com/support/solutions/articles/247929-can-i-change-the-time-on-quotes-and-charts-to-reflect-local-time-
  # For Equities, the quotes and charts will be in Eastern timezone. For Futures and Forex, it will be quoted in Central timezone.

  barchart_connect()

  # symbol = 'IEF'; tzone = 'America/New_York'; from = '2023-01-02 10:00:00'; to = '2023-01-02 11:59:59'; interval = 1
  # symbol = 'BTC'; tzone = 'America/New_York'; from = '2023-01-02 10:00:00'; to = '2023-01-02 11:59:59'; interval = 1
  # symbol = '^BTCUSD'

  start = as.POSIXct( from, tz = tzone )
  end = as.POSIXct( to, tz = tzone )

  time_format = '%Y%m%d%H%M%S'

  url = httr::modify_url(
    url = 'https://www.barchart.com', path = if( extended ) 'proxies/timeseries/queryformtminutes.ashx' else 'proxies/timeseries/queryminutes.ashx',
    query = list(
      symbol = symbol, interval = interval,
      start = format( start, time_format ), end = format( end, time_format ),
      volume = 'contract', order = 'asc', dividends = F,  backadjust = F, daystoexpiration = 1, contractroll = 'expiration'
    ) )

  x = rvest::session_jump_to( barchart.globals$session, url, barchart.globals$header_token )
  content = httr::content( x$response )

  if( is.null( content ) ) return( NULL )
  if( grepl( 'error', ignore.case = T, httr::content( x$response, 'text', encoding = 'UTF-8' ) ) ) stop( 'Barchart says: ', content )

  candles = fread( content )
  setnames( candles, c( 'time', 'day', 'open', 'high', 'low', 'close', 'volume' ) )
  candles[, time := as.POSIXct( time, tz = tzone ) ]
  candles[]

}
#' @export
barchart_get_split_and_dividends = function( symbol, from, to ) {

  barchart_connect()

  # symbol = 'IEF'; from = '2000-01-02 10:00:00'; to = '2023-01-02 11:59:59'
  # symbol = 'AAPL'; from = '2000-01-02 10:00:00'; to = '2023-01-02 11:59:59'


  start = lubridate::as_date( from )
  end = lubridate::as_date( to )

  date_format = '%Y%m%d'

  url = httr::modify_url(
    url = 'https://www.barchart.com', path = 'proxies/timeseries/queryevents.ashx',
    query = list(
      symbols = symbol,
      start = format( start, date_format ), end = format( end, date_format ),
      dividends = T,  earnings = F, splits = T, custom = F
    ) )

  x = rvest::session_jump_to( barchart.globals$session, url, barchart.globals$header_token )
  content = httr::content( x$response )

  if( is.null( content ) ) return( NULL )
  events = fread( content )
  setnames( events, c( 'symbol', 'date', 'event', 'value' ) )
  events[, date := as.Date( date ) ]
  events[]

}
#' @export
barchart_get_eod = function( symbol, from, to, interval ) {

  barchart_connect()

  # symbol = 'IEF'; tzone = 'America/New_York'; from = '2023-01-02 10:00:00'; to = '2023-01-02 11:59:59'; interval = 1
  # symbol = 'IEF'; tzone = 'America/New_York'; from = '2021-01-02 10:00:00'; to = '2023-01-02 11:59:59'; interval = 1
  # symbol = 'ESM3|3000C'; tzone = 'America/New_York'; from = '2023-01-02 10:00:00'; to = '2023-03-02 11:59:59'; interval = 1

  start = lubridate::as_date( from )
  end = lubridate::as_date( to )

  date_format = '%Y%m%d'
  url = httr::modify_url(
    url = 'https://www.barchart.com', path = 'proxies/timeseries/queryeod.ashx',
    query = list(

      symbol = symbol, data = 'daily',
      start = format( start, date_format ), end = format( end, date_format ),
      volume = 'contract', order = 'asc', dividends = F,  backadjust = F, daystoexpiration = 1, contractroll = 'expiration'
    ) )

  x = rvest::session_jump_to( barchart.globals$session, url, barchart.globals$header_token )
  content = httr::content( x$response )

  if( is.null( content ) ) return( NULL )
  if( grepl( 'error', ignore.case = T, content ) ) stop( 'Barchart says: ', content )
  candles = fread( content )[, 1:7]
  setnames( candles, c( 'symbol', 'date', 'open', 'high', 'low', 'close', 'volume' ) )
  candles[, date := as.Date( date ) ]
  candles[]

}
#' @export
barchart_search_symbols = function( query ) {

  # query = 'Treasury Bond Ishares'
  x = httr::GET( httr::modify_url( 'https://instruments-prod.aws.barchart.com', path = c( '/instruments/search', query ) ) )
  x = httr::content( x )$instruments
  rbindlist( lapply( x, function( x ) x[ sapply( x, length ) == 1 ] ), fill = T )

}
#' @export
barchart_search_symbols_advanced = function( query, limit = 50 ) {

  barchart_connect()

  url = httr::modify_url( 'https://www.barchart.com/', path = 'proxies/core-api/v1/search', query = list(

    q = query,
    fields = 'symbol,symbolName,exchange,symbolCode,symbolType,lastPrice,dailyLastPrice',
    meta = 'field.shortName,field.description',
    limit = limit,
    searchType = 'contains',
    assetClasses = 'equities,indices,etfs,mutual_funds,futures,futures_options,forex,crypto,rates,sectors', searchName = 1,raw = 1,
    regions = 'us'

  ) )
  x = rvest::session_jump_to( barchart.globals$session, url, barchart.globals$header_token )
  content = httr::content( x$response )

  if( is.null( content ) ) return( NULL )
  if( grepl( 'error', ignore.case = T, httr::content( x$response, 'text', encoding = 'UTF-8' ) ) ) stop( 'Barchart says: ', content )

  rbindlist( lapply( content$data, function( x ) x$raw ) )

}

barchart.globals = new.env()

if( F ) {

  ## list all searchable symbols
  available_symbols = barchart_search_symbols_advanced( '', limit = 100000 )
  available_symbols[, unique( exchange ) ]
  available_symbols[, .N, by = exchange ]

  ## search symbol
  barchart_search_symbols( 'ief' )
  barchart_search_symbols_advanced( 'spy' )

  ## get intraday data

  # note
  # https://help.barchart.com/support/solutions/articles/247929-can-i-change-the-time-on-quotes-and-charts-to-reflect-local-time-
  # For Equities, the quotes and charts will be in Eastern timezone. For Futures and Forex, it will be quoted in Central timezone.

  stock   = barchart_get_intraday( 'SPY'  , '2021-01-04', '2021-01-05', tz = 'America/New_York', 1 )
  futures = barchart_get_intraday( 'ESM21', '2021-01-04', '2021-01-05', tz = 'America/Chicago' , 1 )
  index   = barchart_get_intraday( '$INX' , '2021-01-04', '2021-01-05', tz = 'America/New_York', 1 )

  plot_dts( stock[, .( time, stock = open / open[1] ) ], futures[ , .( time, futures = open / open[1] ) ], index[ , .( time, index = open / open[1] ) ] )

  stock    = barchart_get_intraday( 'BITO'   , '2023-03-06', '2023-03-07', tz = 'America/New_York', 1 )
  futures  = barchart_get_intraday( 'BAH23'  , '2023-03-06', '2023-03-07', tz = 'America/Chicago' , 1 )
  currency = barchart_get_intraday( '^BTCUSD', '2023-03-06', '2023-03-07', tz = 'America/Chicago' , 1 )

  plot_dts( stock[, .( time, stock = open / open[1] ) ], futures[ , .( time, futures = open / open[1] ) ], currency[ , .( time, currency = open / open[1] ) ] )

  ## get splits and dividends
  events = barchart_get_split_and_dividends( 'AAPL', '1970-01-01', Sys.Date() )

  ## get daily data
  candles = barchart_get_eod( '$DSTQ', '1970-01-01', Sys.Date() )
  plot_dts( candles[, .( date, open ) ] )

  ## get expired option data
  candles = barchart_get_eod( 'ESM0|3000C', '1970-01-01', Sys.Date() )
  plot_dts( candles[, .( date, open ) ] )

  ## get intraday extended data
  candles_reg = barchart_get_intraday( 'AAPL', '2023-03-21', '2023-03-22', tz = 'America/New_York', 1, extended = F )
  candles_ext = barchart_get_intraday( 'AAPL', '2023-03-21', '2023-03-22', tz = 'America/New_York', 1, extended = T )

  plot_dts( candles_ext[ , .( time, extended = open ) ], candles_reg[, .( time, regular = open ) ] )


}
