#' Returns a random bool
#'
#' @param likelihood sets the likelihood of returning TRUE from 0-100
#' @export
#' @examples
#' chancer.bool()
#' chancer.bool(10)
chancer.bool <- function(likelihood=50){
  
  if(likelihood < 0 || likelihood > 100){
    stop("chancer.bool accepts likelihood values from 0 to 100")
  }
  
  return(runif(1) * 100 <= likelihood)
}

#' Get a random character
#'
#' @param case select 'uppercase', 'lowercase' or 'all' for both cases
#' @param pool select 'alpha' for just letters, 'numeric' for just numbers, 
#' 'symbol' for just symbols or 'all' for all 
#' @param user pass in string to select from your own pool of chars otherwise 
#' leave NULL. If this is not NULL then case and pool do nothing
#' @export
#' @examples
#' chancer.character('all', 'symbol')
#' chancer.character(user='i_am_a_test_string')
#' 
chancer.character <- function(case=c('all', 'lower', 'upper'), 
                             pool=c('all', 'alpha', 'numeric', 'symbol'),
                             user=NULL){
  
  # check what case to use
  casing <- match.arg(case)  
  if(casing == 'upper'){
    letters <- CHARS_UPPER
  }else if(casing == 'lower'){
    letters <- CHARS_LOWER
  }else if(casing == 'all'){
    letters <- paste0(CHARS_UPPER, CHARS_LOWER)
  }else{
    stop("casing must be one of 'upper', 'lower' or 'all'")
  }
  
  # check what pool of chars to use  
  pooling <- match.arg(pool)
  if(pooling == 'all'){
    pool <- paste0(letters, SYMBOLS)
  }else if(pool=='alpha'){
    pool <- letters
  }else if(pool=='numeric'){
    pool <- NUMBERS
  }else if(pool=='symbol'){
    pool <- SYMBOLS
  }
  
  # are we using user pool of chars?
  if(!is.null(user)){
    pool <- user
  }
  
  # pick the random chars
  tmp <- chancer.pick(1:nchar(pool))
  
  # and return
  return(substr(pool, tmp, tmp))
}

#' Get a random floating point number
#'
#' @param min minimum possible value
#' @param max maximum possible value
#' @param fixed maximum number of decimal place to allow. Note may return 
#' fewer decimal places, fixed is not guaranteed
#' @export
#' @examples
#' chancer.floating(0, 1, 2)
#' 
chancer.floating <- function(min=0, max=100, fixed=4){
  
  if(min > max){
    stop('min must be smaller than max')
  }
  
  return(round(runif(1, min, max), fixed))
}

#' Get a random integer
#'
#' @param min minimum possible value
#' @param max maximum possible value
#' @export
#' @examples
#' chancer.integer()
#' chancer.integer(1, 25)
#' 
chancer.integer <- function(min=MIN_INT, max=MAX_INT){
  
  if(min > max){
    stop('min must be smaller than max')
  }
  
  return(as.integer(runif(1, min, max) + 0.5))
}


#' Get a random natural number
#'
#' @param min minimum possible value
#' @param max maximum possible value
#' @export
#' @examples
#' chancer.natural()
#' chancer.natural(1, 25)
#' 
chancer.natural <- function(min=1, max=100){
  return(chancer.integer(min, max))
}

#' Get a random string
#'
#' @param len length of string to return
#' @param case select 'uppercase', 'lowercase' or 'all' for both cases
#' @param pool select 'alpha' for just letters, 'numeric' for just numbers, 
#' 'symbol' for just symbols or 'all' for all 
#' @param user if null string contains chars from 
#' 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@@#$%^&*()'
#' else you can pass in a user defined string of chars
#' @export
#' @examples
#' chancer.string(len=10, user='xyz')
#' chancer.string(len=10, case='lower', pool='alpha')
#' chancer.string(len=10, pool='symbol')
#' 
chancer.string <- function(len=10, case='all', pool='all', user=NULL){
  
  chars <- sapply(1:len, function(x){
    chancer.character(case=case, pool=pool, user=user)
  })
  
  chars <- paste0(chars, collapse="", sep="")
  
  return(chars)
}

