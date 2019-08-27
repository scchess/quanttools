investing_get_data = function( id, from, to ) {

  # https://github.com/derlin/investing-historical-data/blob/master/investing-get.js
  response = httr::POST( 
    url = 'https://uk.investing.com/instruments/HistoricalDataAjax', 
    httr::add_headers( 
      'Origin' = 'http://www.investing.com',
      'X-Requested-With' = 'XMLHttpRequest'
    ),
    httr::user_agent(
      'https://bitbucket.org/quanttools/quanttools'
      # paste( 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu', 'Chromium/51.0.2704.79 Chrome/51.0.2704.79 Safari/537.36' )
    ),
    body = list(
      'action' = 'historical_data', 
      'curr_id' = id, 
      'st_date' = format( as.Date( from ), '%m/%d/%Y' ), 
      'end_date' = format( as.Date( to ), '%m/%d/%Y' ), 
      'interval_sec' = 'Daily'
    )
  )
  
  table = xml2::xml_find_all( httr::content( response ), './/table[@id="curr_table"]' )
  data = xml2::xml_find_all( table, './/td[@data-real-value]' )
  
  data = as.data.table( 
    matrix( 
      as.numeric( gsub( ',', '', xml2::xml_attr( data, 'data-real-value' ), fixed = T ) ), 
      ncol = 6, 
      byrow = T, 
      dimnames = list( 
        NULL, 
        c( 'date', 'close', 'open', 'high', 'low', 'volume' ) 
      ) 
    )
  )
  data[, date := as.Date( as.POSIXct( date, origin = '1970-01-01', tz = 'UTC' ) ) ]
  setorder( data, date )
  setcolorder( data, c( 'date', 'open', 'high', 'low', 'close' ) )
  return( data[] )
  
}
investing_get_info = function( url ) {
  
  response = httr::GET( url ) #, httr::add_headers( 
    #'Origin' = 'http://www.investing.com' ), httr::user_agent( paste( 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu', 'Chromium/51.0.2704.79 Chrome/51.0.2704.79 Safari/537.36' ) ) )

  pairs = xml2::xml_find_all( httr::content( response ), './/tr[starts-with(@id,"pair_")]' )
  
  flags         = xml2::xml_find_all( pairs, './/td[@class="flag"]/span' )
  url_and_title = xml2::xml_find_all( pairs, './/a[@href]' )
  symbol_and_id = xml2::xml_find_all( pairs, './/span[@data-name]' )
  
  setDT( c( 
    lapply( c( name = 'data-name', id = 'data-id' ), xml2::xml_attr, x = symbol_and_id ),
    lapply( c( title = 'title', url = 'href' ), xml2::xml_attr, x = url_and_title ),
    lapply( c( country = 'title' ), xml2::xml_attr, x = flags )
  ) )[]

}
investing_search = function( query ) {
  
  url = httr::modify_url( 'https://www.investing.com/search/', query = list( q = query ) )
  response = httr::GET( url )
  message( response$url )
  content  = httr::content( response, as = 'text' )
  result = unlist( regmatches( content, gregexpr( '(?<=window.allResultsQuotesDataArray = ).+?(?=;\n)', content, perl = T ) ) )
  jsonlite::fromJSON( result )
  
}

if( F ) {

url = 'https://www.investing.com/indices/world-indices'
url = 'https://www.investing.com/indices/european-indices'
url = 'https://www.investing.com/indices/european-indices?&majorIndices=on&primarySectors=on&additionalIndices=on&otherIndices=on'
info = investing_get_info( url )
info

url = 'https://www.investing.com/equities/united-states'
url = 'https://www.investing.com/equities/StocksFilter?noconstruct=1&smlID=800&sid=&tabletype=price&index_id=166' # S&P 500
info = investing_get_info( url )
info

investing_search( 'stoxx 50' )
investing_search( 'US0378331005' )
investing_search( 'Apple' )

from = '1990-01-01'
to   = '2020-01-01'
id = 6408   # Apple Inc
id = 175    # Euro Stoxx 50

candles = investing_get_data( id, from, to )

plot_dts( candles[, .( date, close ) ] )

}