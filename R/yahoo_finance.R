
## ---- crumb version ----
yahoo_finance_env = new.env()

yahoo_finance_update_crumb_and_handle = function() {

  message( 'generating new crumb' )

  handle = httr::handle( 'https://finance.yahoo.com/quote' )
  response = httr::GET( handle = handle )
  crumb = gsub( '^.*crumb":"\\s*|".*', '', httr::content( response, 'text' ) )

  assign( 'crumb' , crumb , envir = yahoo_finance_env )
  assign( 'handle', handle, envir = yahoo_finance_env )

}

yahoo_finance_generate_export_url = function( symbol, from, to, event = c( 'history', 'split', 'div' ), interval = '1d' ) {

  event = match.arg( event )

  url_parameters = list(

    period1 = as.numeric( min( Sys.Date(), as.Date( from )     ), units = 'day' ) * 24 * 60 * 60,
    period2 = as.numeric( min( Sys.Date(), as.Date( to   ) + 1 ), units = 'day' ) * 24 * 60 * 60,
    interval = interval,
    events = event,
    crumb = yahoo_finance_env$crumb

  )
  # create get query url
  url = paste( paste0( 'https://query1.finance.yahoo.com/v7/finance/download/', gsub( '=', '%3D', symbol ) ), paste( names( url_parameters ), url_parameters, sep = '=', collapse = '&' ), sep = '?' )

  return( url )

}

yahoo_finance_download_data = function( symbol, from, to, event ) {

  trial = 0
  while( T ) {

    trial = trial + 1
    if( trial == 4 ) stop( 'can\'t download data, tried 3 times', .Call = F )

    url = yahoo_finance_generate_export_url( symbol, from, to, event )

    response = httr::GET( url, handle = yahoo_finance_env$handle )

    if( !httr::http_error( response ) ) {

      content = httr::content( response, as = 'text' )
      break

    } else {

      content = httr::content( response )
      error_code = content$finance$error$code

      if( !is.null( error_code ) && error_code == "Unauthorized" ) {

        yahoo_finance_update_crumb_and_handle()

      }

    }

  }

  colnames   = c( 'date'     , switch( event, history = c( 'open'   , 'high'   , 'low'    , 'close'  , 'adj_close', 'volume'  ), split = 'split'    , div = 'dividends' ) )
  colclasses = c( 'character', switch( event, history = c( 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'  , 'numeric' ), split = 'character', div = 'numeric'   ) )

  data = fread( content, col.names = colnames, colClasses = colclasses )
  data[, date := as.Date( date ) ]

  if( event == 'split' ) data[, split := sapply( parse( text = split ), eval ) ]

  return( data[] )

}

## ---- root.App.main version ----
yahoo_finance_get = function( symbol, from, to, what = c( 'financials', 'history' ) ) {

  what = match.arg( what )

  period1 = as.numeric( min( Sys.Date(), as.Date( from )     ), units = 'day' ) * 24 * 60 * 60
  period2 = as.numeric( min( Sys.Date(), as.Date( to   ) + 1 ), units = 'day' ) * 24 * 60 * 60

  url = paste0(
    'https://finance.yahoo.com/quote/', gsub( '=', '%3D', symbol ), '/', what, '?p=', gsub( '=', '%3D', symbol ),
    '&period1=', format( period1, scientific = F ),
    '&period2=', format( period2, scientific = F ), '&lang=en-US&region=US' )

  response = httr::GET( url )

  html = httr::content( response, 'text' )
  data_json = unlist( regmatches( html, gregexpr( "(?<=root.App.main\\s=\\s).+?(?=;\n)", html, perl = T ) ) )

  data = jsonlite::fromJSON( data_json )

  return ( data$context$dispatcher$stores[[ switch( what, financials = 'QuoteSummaryStore' , history = 'HistoricalPriceStore' ) ]] )

}

## ---- public interface ----

# download market data from Yahoo server
#' @rdname get_market_data
#' @export
get_yahoo_data = function( symbol, from, to, split.adjusted = TRUE, dividend.adjusted = TRUE ) {

  dat = yahoo_finance_download_data( symbol, from, to, event = 'history' )

  if( is.null( dat ) || nrow( dat ) == 0 ) return( dat )

  effective_split = high = low = split_coeff = split_date = stock_splits = volume = NULL

  if( split.adjusted ) {

    splits = yahoo_finance_download_data( symbol, from, to, event = 'split' )

    dat[ , split_coeff := 1 ]
    if( !is.null( splits ) ) {

      splits = splits[, list( split_date = date, split ) ]
      # filter out already adjusted splits ( yahoo finance bug )
      splits[, effective_split := dat[ which( date == split_date ) + -1:0 ][, close[1] / open[2] ], by = split_date ]

      if( nrow( splits ) > 0 ) {

        splits = splits[ abs( effective_split / split - 1 ) < 0.05 ]

        splits[ , dat[ date < split_date, split_coeff := split_coeff * split ][ NULL ], by = seq_along( splits$split_date ) ]

        dat[ , ':='(

          open   = open   / split_coeff,
          high   = high   / split_coeff,
          low    = low    / split_coeff,
          close  = close  / split_coeff,
          volume = volume * split_coeff

        ) ]

      }

    }

  }
  if( dividend.adjusted ) {

    dividends = yahoo_finance_download_data( symbol, from, to, event = 'div' )

    div = div_coeff = div_date = NULL

    dat[ , div_coeff := 1 ]
    if( !is.null( dividends ) ) {

      dividends = dividends[, list( div_date = date, div = dividends ) ]

      if( nrow( dividends ) > 0 ) {

        # https://help.yahoo.com/kb/SLN28256.html
        dividends[ , dat[ date < div_date, div_coeff := div_coeff * ( 1 - div / close[.N] ) ], by = seq_along( dividends$div_date ) ]

        dat[ , ':='(

          open   = open   * div_coeff,
          high   = high   * div_coeff,
          low    = low    * div_coeff,
          close  = close  * div_coeff,
          volume = volume / div_coeff

        ) ]

      }

    }

  }

  # return downloaded data
  return( dat[] )

}

#' @rdname get_market_data
#' @export
get_yahoo_splits_and_dividends = function( symbol, from, to = from ) {

  data      = yahoo_finance_download_data( symbol, from, to, event = 'history' )
  if( is.null( data ) ) return( NULL )
  dividends = yahoo_finance_download_data( symbol, from, to, event = 'div'     )
  splits    = yahoo_finance_download_data( symbol, from, to, event = 'split'   )

  . = event = NULL
  splits   [, event := rep( 'split'    , .N ) ]
  dividends[, event := rep( 'dividends', .N ) ]

  names = c( 'date', 'value', 'event' )
  setnames( splits   , names )
  setnames( dividends, names )

  rbind( splits, dividends )[ order( date ) ]

}

## ---- experimental ----
experimental_get_yahoo_data = function( symbol, from, to, split.adjusted = TRUE, dividend.adjusted = TRUE ) {

  data = yahoo_finance_get( symbol, from, to, what = 'history' )

  if( is.null( data ) ) return( NULL )

  prices = data.table( data$prices     )

  prices[, date := as.Date( as.POSIXct( date, origin = '1970-01-01', tz = 'US/Eastern' ) ) ]
  setorder( prices, date )

  splits = dividends = NULL
  if( !is.null( data$eventsData ) && length( data$eventsData ) > 0 ) {

    type = denominator = numerator = amount = NULL
    prices = prices[ is.na( type ) ]
    events = data.table( data$eventsData )
    events[, date := as.Date( as.POSIXct( date, origin = '1970-01-01', tz = 'US/Eastern' ) ) ]
    setorder( events, date )

    if( events[, 'SPLIT'    %in% unique( type ) ] ) splits    = events[ type == 'SPLIT'   , list( date, split     = denominator / numerator ) ]
    if( events[, 'DIVIDEND' %in% unique( type ) ] ) dividends = events[ type == 'DIVIDEND', list( date, dividends = amount                  ) ]

  }

  effective_split = high = low = split_coeff = split_date = stock_splits = volume = adjclose = NULL

  prices = prices[, list( date, open, high, low, close, adj_close = adjclose, volume ) ]

  if( split.adjusted ) {

    prices[ , split_coeff := 1 ]
    if( !is.null( splits ) ) {

      splits = splits[, list( split_date = date, split ) ]
      # filter out already adjusted splits ( yahoo finance bug )
      splits[, effective_split := prices[ which( date == split_date ) + -1:0 ][, close[1] / open[2] ], by = split_date ]

      if( nrow( splits ) > 0 ) {

        splits = splits[ abs( effective_split / split - 1 ) < 0.05 ]

        splits[ , prices[ date < split_date, split_coeff := split_coeff * split ][ NULL ], by = seq_along( splits$split_date ) ]

        prices[ , ':='(

          open   = open   / split_coeff,
          high   = high   / split_coeff,
          low    = low    / split_coeff,
          close  = close  / split_coeff,
          volume = volume * split_coeff

        ) ]

      }

    }

  }
  if( dividend.adjusted ) {

    div = div_coeff = div_date = NULL

    prices[ , div_coeff := 1 ]
    if( !is.null( dividends ) ) {

      dividends = dividends[, list( div_date = date, div = dividends ) ]

      if( nrow( dividends ) > 0 ) {

        # https://help.yahoo.com/kb/SLN28256.html
        dividends[ , prices[ date < div_date, div_coeff := div_coeff * ( 1 - div / close[.N] ) ], by = seq_along( dividends$div_date ) ]

        prices[ , ':='(

          open   = open   * div_coeff,
          high   = high   * div_coeff,
          low    = low    * div_coeff,
          close  = close  * div_coeff,
          volume = volume / div_coeff

        ) ]

      }

    }

  }

  # return downloaded data
  return( prices[] )

}

query_modules = function( symbol ) {

  # https://stackoverflow.com/a/47505102

  modules = c(

    'assetProfile',
    'incomeStatementHistory',
    'incomeStatementHistoryQuarterly',
    'balanceSheetHistory',
    'balanceSheetHistoryQuarterly',
    'cashflowStatementHistory',
    'cashflowStatementHistoryQuarterly',
    'defaultKeyStatistics',
    'financialData',
    'calendarEvents',
    'secFilings',
    'recommendationTrend',
    'upgradeDowngradeHistory',
    'institutionOwnership',
    'fundOwnership',
    'majorDirectHolders',
    'majorHoldersBreakdown',
    'insiderTransactions',
    'insiderHolders',
    'netSharePurchaseActivity',
    'earnings',
    'earningsHistory',
    'earningsTrend',
    'industryTrend',
    'indexTrend',
    'sectorTrend'

  )

  url = httr::modify_url(

    url  = 'https://query2.finance.yahoo.com',
    path = c( '/v10/finance/quoteSummary', symbol ),
    query = list( modules = paste( modules, collapse = ',' ) )

  )

  response = httr::GET( url, httr::user_agent( 'https://bitbucket.org/quanttools/quanttools' ) )

  if( httr::http_type( response ) != 'application/json' ) {

    stop( 'API did not return json', call. = FALSE )

  }

  data   = jsonlite::fromJSON( httr::content( response, 'text' ), simplifyVector = FALSE )$quoteSummary
  result = data$result[[1]]
  error  = data$error

  if( httr::http_error( response ) ) {

    stop( sprintf( "Yahoo API request failed [%s]\n%s\n<%s>", httr::status_code( response ), data$error$description, url ), call. = FALSE )

  }

  result

}

query_prices = function( symbol, from, to ) {

  # https://stackoverflow.com/a/47505102

  url = httr::modify_url(

    url   = 'https://query2.finance.yahoo.com',
    path  = c( '/v8/finance/chart', symbol ),
    query = list(

      symbol   = symbol,
      period1  = as.numeric( min( Sys.Date(), as.Date( from )     ), units = 'day' ) * 24 * 60 * 60,
      period2  = as.numeric( min( Sys.Date(), as.Date( to   ) + 1 ), units = 'day' ) * 24 * 60 * 60,
      interval = '1d',
      events   = 'div,split'

    )

  )

  response = httr::GET( url, httr::user_agent( 'https://bitbucket.org/quanttools/quanttools' ) )

  if( httr::http_type( response ) != 'application/json' ) {

    stop( 'API did not return json', call. = FALSE )

  }

  data   = jsonlite::fromJSON( httr::content( response, 'text' ), simplifyVector = FALSE )$chart
  result = data$result[[1]]
  error  = data$error

  if( httr::http_error( response ) ) {

    stop( sprintf( "Yahoo API request failed [%s]\n%s\n<%s>", httr::status_code( response ), data$error$description, url ), call. = FALSE )

  }

  timestamp = data$timestamp
  time_zone = data$meta$exchangeTimezoneName
  ohlcv     = data$indicators$quote[[1]]
  splits    = data$events$splits
  dividends = data$events$dividends

  timestamp_to_date = function( timestamp ) as.Date( as.POSIXct( unlist( timestamp ), origin = '1970-01-01', tz = time_zone ) )

  if( !is.null( ohlcv ) ) {
    ohlcv = setDT( lapply( ohlcv, unlist ) )
    ohlcv[, date := date_from_yahoo( timestamp ) ]
    setcolorder( ohlcv, c( 'date', 'open', 'high', 'low', 'close' ) )[]
  }

  if( !is.null( dividends ) ) {
    dividends = setDT( lapply( list( date = 'date', dividends = 'amount' ), function( x ) sapply( dividends, '[[', x ) ) )[ order( date ) ]
    dividends[, date := timestamp_to_date( date ) ][]
  }

  if( !is.null( splits ) ) {
    splits = setDT( lapply( list( date = 'date', num = 'numerator', denom = 'denominator' ), function( x ) sapply( splits, '[[', x ) ) )[ order( date ) ]
    splits[, ':='( date = timestamp_to_date( date ), split = denom / num, denom = NULL, num = NULL ) ][]
  }

  ohlcv

}

if( F ) {

  symbol = 'AAPL'
  from = '2001-01-01'
  to = '2004-01-01'

  data = query_modules( symbol )
  system.time( { data = query_prices ( symbol, from = '1990-01-01', to = '2019-08-01' ) } )

}

