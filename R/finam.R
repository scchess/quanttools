finam_get_emitent_data = function() {

  source_url = 'https://www.finam.ru/cache/N72Hgd54/icharts/icharts.js'

  #Sys.setlocale( 'LC_CTYPE', 'ru_RU.UTF-8' )

  aEmitentChild = aEmitentUrls = aEmitentDecp = NULL

  # read finam universe file, set encoding to UTF-8 and split lines
  raw_lines = unlist( strsplit( gsub( '"', '', iconv( rawToChar( httr::content( httr::GET( source_url, httr::config( http_version = 0 ) ) ) ), 'CP1251', 'UTF-8' ) ), '\n|\r' ) )
  # filter out empty lines
  raw_lines = raw_lines[ raw_lines != '' ]
  # get data inside square and curly brakets for each line
  data_inside_brakets = unlist( regmatches( raw_lines, gregexpr( "(?<=\\[|\\{).+?(?=\\]|\\})", raw_lines, perl = T ) ) )
  # get data names
  var_names = unlist( regmatches( raw_lines, gregexpr( "(?<=var\\s).+?(?=\\s)", raw_lines, perl = T ) ) )
  # combine all data into list
  data = c(
    # all data except aEmitentNames has no comma (,) inside element
    strsplit( data_inside_brakets[ var_names != 'aEmitentNames' ], ',', fixed = T ),
    # but aEmitentNames has the comma (,) inside element
    {
      x = strsplit( data_inside_brakets[ var_names == 'aEmitentNames' ], '' )[[1]]
      # so we split by "','" and remove first and last quote mark '
      strsplit( paste( x[ 2:( length( x ) - 1 ) ], collapse = '' ), split = "','", fixed = T )
    }
  )
  # set data names
  names( data ) = c( var_names[ var_names != 'aEmitentNames' ], 'aEmitentNames' )
  # check if data is Emitent specific
  is_emitent_data = sapply( names( data ), grepl, pattern = 'Emitent' )
  # combine Emitent specific data into data.table
  emitent_data = setDT( data[ is_emitent_data ] )[ aEmitentChild != 0 ]
  # clean aEmitentUrls
  emitent_data[, aEmitentUrls := matrix( unlist( strsplit( aEmitentUrls, ': ', fixed = T ) ), ncol = 2, byrow = T )[, 2 ] ]
  # split aEmitentUrls into market and symbol parts
  emitent_data[ , c( 'aEmitentUrlsMarket', 'aEmitentUrlsSymbol' ) := as.data.table( matrix( unlist( strsplit( aEmitentUrls, '/' ) ), ncol = 2, byrow = T ) ) ]
  # clean aEmitentDecp
  emitent_data[ , aEmitentDecp := matrix( unlist( strsplit( aEmitentDecp, ':' ) ), ncol = 2, byrow = T )[, 2 ] ]

  return( emitent_data[] )

}

finam_scrap_symbol_map = function() {

  get_emitent_info = function( emitent_url ) {

    response = httr::GET( paste0( 'https://www.finam.ru/profile/', emitent_url, '/export' ), httr::config( http_version = 0, referer = 'https://www.finam.ru/analysis/profile041CA00007/default.asp' ) )
    content  = httr::content( response )

    inputs = xml2::xml_find_all( content, '//form[@name="exportdata"]/input' )
    attributes = xml2::xml_attrs( inputs )

    values = sapply( attributes, '[', 'value' )
    names( values ) = sapply( attributes, '[', 'name' )
    return( values )

    # extended info
    if( F ) {
      content  = httr::content( response, as = 'text' )
      issue = unlist( regmatches( content, gregexpr( '(?<=Finam.IssuerProfile.Main.issue = ).+?(?=;\r)', content, perl = T ) ) )
      issue = gsub( ',\\s?\\}', '\\}', issue )
      issue = gsub( '\t', '\\s', issue )
      jsonlite::fromJSON( issue )$quote[ c( 'id', 'code', 'fullUrl', 'title', 'linkedList' ) ]
    }

  }

  emitent_data = finam_get_emitent_data()

  info = vector( nrow( emitent_data ), mode = 'list' )

  pb = progress::progress_bar$new( format = "  downloading [:bar] :percent eta: :eta", total = length( info ), clear = F, width = 100 )

  for( i in seq_along( info ) ) {

    while( length( info[[i]] ) == 0 ) info[[i]] = get_emitent_info( emitent_data$aEmitentUrls[i] )
    pb$tick()

  }

  # which_empty = which( sapply( info , length ) == 0 )

  info = rbindlist( lapply( info, as.list ) )

  code = aEmitentNames = aEmitentUrls = market = em = symbol = NULL

  symbol_map = data.table( emitent_data, info )[ !duplicated( code ), list( name = aEmitentNames, url = aEmitentUrls, market = as.numeric( market ), em = as.numeric( em ), symbol = code ) ][]
  setkey( symbol_map, symbol )

  finam_symbol_map = symbol_map
  # usethis::use_data( finam_symbol_map, internal = TRUE )

  return( symbol_map )

}

finam_generate_export_url = function( em, from, to, p ) {

  # split dates into parts
  date_range = lapply( c( from, to ), function( date ) as.numeric( format( date, c( '%Y', '%m', '%d' ) ) ) )

  url_parameters = list(

    fsp       = 0, # fill periods without deals 0 - no, 1 - yes
    em        = em, # emitent code
    df        = date_range[[1]][3], # date from
    mf        = date_range[[1]][2] - 1,
    yf        = date_range[[1]][1],
    dt        = date_range[[2]][3], # date to
    mt        = date_range[[2]][2] - 1,
    yt        = date_range[[2]][1],
    p         = p, # period
    dtf       = 1, # date format
    tmf       = 3, # time format
    MSOR      = 1, # candle time 0 - candle start, 1 - candle end
    mstimever = 0,
    sep       = 1, # column separator    1 - ",", 2 - ".", 3 - ";", 4 - "<tab>", 5 - " "
    sep2      = 1, # thousands separator 1 - "" , 2 - ".", 3 - ",", 4 - " "    , 5 - "'"
    datf      = ifelse( p == 1, 9, 5 ), # candle format
    at        = 0 # header	0 - no, 1 - yes

  )
  # create get query url
  url = paste( 'http://export.finam.ru/export', paste( names( url_parameters ), url_parameters, sep = '=', collapse = '&' ), sep = '?' )

  return( url )

}


finam_download_data = function( symbol, from, to = from, period = c( 'day', '1min', '5min', '10min', '15min', '30min', 'hour', 'tick' ), trial_limit = 10, trial_sleep = 0.5, verbose = T ) {

  args = as.list( environment() )

  from = min( as.Date( from ), Sys.Date() )
  to   = min( as.Date( to   ), Sys.Date() )

  is_split_ticks = period == 'tick' & from != to
  is_split_1min  = period == '1min' & to - from > as.difftime( 31, units = 'days' )
  is_split = is_split_ticks | is_split_1min

  if( is_split ) {

    # if daily data not available then higher resolution not available
    daily = do.call( finam_download_data, within( args, { period = 'day' } ) )
    if( is.null( daily ) ) return( NULL )

    if( is_split_ticks ) {

      if( verbose ) message( 'tick data requested for more than one date, request will be processed by date:' )
      if( verbose ) message( paste( daily$date, collapse = ', ' ) )

      data = vector( mode = 'list', length( daily$date ) )

      pb = progress::progress_bar$new( format = "  downloading [:bar] :percent eta: :eta elapsed :elapsed", total = length( data ) )
      for( i in seq_along( data ) ) { data[[ i ]] = do.call( finam_download_data, within( args, { from = to = daily$date[i] } ) ); pb$tick() }

    }

    if( is_split_1min ) {

      from = daily[  1, date ]
      to   = daily[ .N, date ]

      months = seq( from, to, 'month' )

      froms = c( from, months[-1] )
      tos   = c( months[ -length( months ) ] - 1, to )

      if( verbose ) message( '1min data requested for more than 31 days, request will be processed by month:' )
      if( verbose ) message( paste( froms, tos, collapse = ', ', sep = '/' ) )

      data = vector( mode = 'list', length( froms ) )

      pb = progress::progress_bar$new( format = "  downloading [:bar] :percent eta: :eta elapsed :elapsed", total = length( data ) )
      for( i in seq_along( data ) ) { data[[ i ]] = do.call( finam_download_data, within( args, { from = froms[i]; to = tos[i] } ) ); pb$tick() }

    }

    return( rbindlist( data ) )

  }

  em = finam_symbol_map[ symbol, em ]

  if( is.na( em ) ) stop( symbol, ' symbol not found', call. = F )

  date_range = lapply( c( from, to ), function( date ) as.numeric( format( date, c( '%Y', '%m', '%d' ) ) ) )

  query = list(

    fsp       = 0, # fill periods without deals 0 - no, 1 - yes
    em        = em, # emitent code
    df        = date_range[[1]][3], # date from
    mf        = date_range[[1]][2] - 1,
    yf        = date_range[[1]][1],
    dt        = date_range[[2]][3], # date to
    mt        = date_range[[2]][2] - 1,
    yt        = date_range[[2]][1],
    p         = switch( period, "tick" = 1, "1min" = 2, "5min" = 3, "10min" = 4, "15min" = 5, "30min" = 6, "hour" = 7, "day" = 8 ), # period
    dtf       = 1, # date format
    tmf       = 3, # time format
    MSOR      = 1, # candle time 0 - candle start, 1 - candle end
    mstimever = 0,
    sep       = 1, # column separator    1 - ",", 2 - ".", 3 - ";", 4 - "<tab>", 5 - " "
    sep2      = 1, # thousands separator 1 - "" , 2 - ".", 3 - ",", 4 - " "    , 5 - "'"
    datf      = switch( period, "tick" = 9, 5 ), # candle format
    at        = 0 # header	0 - no, 1 - yes

  )
  # create get query url

  url = httr::modify_url( url  = 'http://export.finam.ru', path = '/export', query = query )

  if( verbose ) message( '\n', url )

  response = httr::RETRY( "GET", url, httr::user_agent( 'https://bitbucket.org/quanttools/quanttools' ), httr::config( http_version = 0 ), times = trial_limit, pause_cap = trial_sleep, pause_min = trial_sleep, quiet = !verbose )

  if( length( response$content ) == 0 ) return( NULL )

  content = httr::content( response, 'text', encoding = "Windows-1251" )

  colnames   = c( 'date'     , 'time'     , switch( period, 'tick' = 'price'  , c( 'open'   , 'high'   , 'low'    , 'close'   ) ), 'volume'  )
  colclasses = c( 'character', 'character', switch( period, 'tick' = 'numeric', c( 'numeric', 'numeric', 'numeric', 'numeric' ) ), 'numeric' )

  if( sum( response$content[ 1:1000 ] == charToRaw( ',' ) ) < length( colnames ) ) {

    old_locale = Sys.getlocale( 'LC_CTYPE' )
    Sys.setlocale( 'LC_CTYPE', 'ru_RU.UTF-8' )
    on.exit( Sys.setlocale( 'LC_CTYPE', old_locale ) )

    stop( content, call. = FALSE )

  }

  data = fread( content, col.names = colnames, colClasses = colclasses )

  # format date and time
  data[, time := fasttime::fastPOSIXct( paste( substr( date, 1, 4 ), substr( date, 5, 6 ), substr( date, 7, 8 ), time ), tz = 'UTC' ) ]
  data[, date := as.Date( time ) ]
  #
  switch( period, 'day' = data[, time := NULL ], data[, date := NULL ] )

  return( data[] )

}

finam_search_symbol = function( x, where = c( 'name', 'symbol' ) ) {

  where = match.arg( where )

  name = symbol = NULL

  finam_symbol_map[ grepl( x, get( where ), ignore.case = T ), list( name, symbol, url ) ]

}


#z = download_finam_data( 'GAZP', from = '2019-07-25', to = '2020-09-01', period = '1min' )

#z = download_finam_data( 'ADR.GAZP', from = '2019-05-25', to = '2020-09-01', period = '1min' )

#z = download_finam_data( 'ADR.GAZP', from = '2019-07-25', to = '2020-09-01', period = 'tick' )

