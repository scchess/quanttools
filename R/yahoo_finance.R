yahoo_query_modules = function( symbol ) {

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

yahoo_query_prices = function( symbol, from, to ) {

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

  timestamp = result$timestamp
  time_zone = result$meta$exchangeTimezoneName
  ohlcv     = result$indicators$quote[[1]]
  adj_close = result$indicators$adjclose[[1]]$adjclose
  splits    = result$events$splits
  dividends = result$events$dividends

  timestamp_to_date = function( timestamp ) as.Date( as.POSIXct( unlist( timestamp ), origin = '1970-01-01', tz = time_zone ) )

  if( !is.null( ohlcv ) ) {

    ohlcv = setDT( lapply( ohlcv, function( x ) as.numeric( unlist( x ) ) ) )
    ohlcv[, date := timestamp_to_date( timestamp )[ !sapply( adj_close, is.null ) ] ]
    ohlcv[, adj_close := as.numeric( unlist( adj_close ) ) ]
    setcolorder( ohlcv, c( 'date', 'open', 'high', 'low', 'close' ) )[]
  }

  if( !is.null( dividends ) ) {
    dividends = setDT( lapply( list( date = 'date', dividends = 'amount' ), function( x ) sapply( dividends, '[[', x ) ) )[ order( date ) ]
    dividends[, date := timestamp_to_date( date ) ][]
  }

  if( !is.null( splits ) ) {
    denum = num = NULL
    splits = setDT( lapply( list( date = 'date', num = 'numerator', denom = 'denominator' ), function( x ) sapply( splits, '[[', x ) ) )[ order( date ) ]
    splits[, ':='( date = timestamp_to_date( date ), split = denom / num, denom = NULL, num = NULL ) ][]
  }

  list( candles = ohlcv, dividends = dividends, split = splits )

}


## ---- public interface ----
#' @rdname get_market_data
#' @export
get_yahoo_splits_and_dividends = function( symbol, from, to = from ) {

  data = yahoo_query_prices( symbol, from, to )

  if( is.null( data$candles ) ) return( NULL )

  events = rbindlist( data[ c( 'split', 'dividends' ) ], idcol = 'event', use.names = F )

  if( nrow( events ) == 0 ) return( NULL )

  names = c( 'event', 'date', 'value' )

  return( setnames( events, names )[ order( date ) ][] )

}

#' @rdname get_market_data
#' @export
get_yahoo_data = function( symbol, from, to, split.adjusted = TRUE, dividend.adjusted = TRUE ) {

  data = yahoo_query_prices( symbol, from, to )

  if( is.null( data$candles ) ) return( NULL )

  prices = data.table( data$candles )

  setorder( prices, date )

  splits    = data$split
  dividends = data$dividends

  effective_split = high = low = split_coeff = split_date = stock_splits = volume = adjclose = NULL

  prices = prices[, list( date, open, high, low, close, adj_close, volume ) ]

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
