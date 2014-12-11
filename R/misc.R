#' Select a random element from a vector
#'
#' @param vec vector to select from
#' @export
#' @examples
#' chancer.pick(c(1, 2, 3, 4))
chancer.pick <- function(vec=NULL){
  
  if(is.null(vec)){
    stop('a vector of data must be provided to select from')
  }
  
  return(sample(vec, 1))
}


#' Selects a random element from a vector using weighted probabilities
#'
#' @param vec vector to select from
#' @export
#' @examples
#' chancer.weighted(c(1, 2, 3), c(0.5, 0.4, 0.1))
chancer.weighted <- function(vec=NULL, probs=NULL){
  
  if(is.null(vec) || is.null(probs) || length(vec) != length(probs)){
    stop('vectors of equal lengths must be supplied for the data and 
         their probabilities')
  }
  
  return(sample(vec, 1, prob=probs))
}

#' Get a random hex hash
#'
#' @param case can be 'upper' or 'lower'
#' @param len length of hash, defaults to 64
#' @export
#' @examples
#' chancer.hash()
#' chancer.hash(case='upper')
chancer.hash <- function(len=64, case='lower'){
  
  if(!case %in% c('lower', 'upper')){
    stop("case must be one of lower or upper")
  }
  
  pool <- HEX_POOL
  
  if(case=='upper'){
    pool <- toupper(pool)
  }
  
  chancer.string(len=len, user=pool)
}


#' Helper function to capitalise first char in string
#'
#' @param word string to capitalise
#' @param len length of hash, defaults to 64
#' @export
#' @examples
#' chancer.hash()
#' chancer.hash(case='upper')
chancer.capitalise <- function(word){
  return(paste0(toupper(substring(word, 1,1)), substring(word, 2, nchar(word))))
}


#' Retun a guid
#'
#' @export
#' @examples
#' chancer.guid()
chancer.guid <- function(){
  
  guid_pool <- "abcdef1234567890"
  variant_pool <- "ab89"
  
  guid <- paste0(chancer.string(8, user=guid_pool), "-",
                 chancer.string(4, user=guid_pool), "-",
                 chancer.string(4, user=guid_pool), "-",
                 chancer.string(4, user=guid_pool), "-",
                 chancer.string(12, user=guid_pool))
  return(guid)
}
