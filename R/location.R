#' Get a random street name
#'
#' @param abbreviation TRUE or FALSE whether to abbreviate street name, 
#' e.g Street to St
#' @export
#' @examples
#' chancer.street.suffix()
#' chancer.street.suffix(abbreviation=TRUE)
chancer.street.suffix <- function(abbreviation=FALSE){
  
  pool <- STREETS$full
  
  if(abbreviation){
    pool <- STREETS$abbreviation
  }
  
  suffix <- chancer.pick(pool)
  
  return(suffix)
}

#' Get a random street name
#'
#' @param abbreviation TRUE or FALSE whether to abbreviate street name, 
#' e.g Street to St
#' @export
#' @examples
#' chancer.street()
#' chancer.street(abbreviation=TRUE)
chancer.street <- function(abbreviation=FALSE){
  
  suffix <- chancer.street.suffix(abbreviation)
  
  prefix <- chancer.word(syllables=2)
  
  street <- chancer.capitalise(paste(prefix, suffix))
  
  return(street)
}

#' Get a random address
#'
#' @param case can be 'upper' or 'lower'
#' @param len length of hash, defaults to 64
#' @export
#' @examples
#' chancer.address()
#' chancer.address(abbreviation=TRUE)
chancer.address <- function(abbreviation=FALSE){
  
  street <- chancer.street(abbreviation)
  
  num <- chancer.natural(5, 2000)
  
  address <- paste(num, street)
  
  return(address)
}

#' Get a random altitude
#'
#' @param max maximum altitude in metres, defaults to 8848 (height of Everest)
#' @export
#' @examples
#' chancer.altitude()
#' chancer.altitude(max=25)
chancer.altitude <- function(max=8848){
  return(chancer.floating(min=0, max=max))
}


#' Get a random area code (US only)
#'
#' @param parens TRUE or FALSE whether to wrap area code in parentheses
#' @export
#' @examples
#' chancer.areacode()
#' chancer.areacode(parens=FALSE)
chancer.areacode <- function(parens=TRUE){
  
  ac <- paste0(chancer.natural(2, 9),
              chancer.natural(0, 8),
              chancer.natural(0, 9))
  
  if(parens){
    ac <- paste0('(', ac, ')')
  }
  
  return(ac)
}


#' Get a random City name
#'
#' @export
#' @examples
#' chancer.city()
chancer.city <- function(){
  return(chancer.capitalise(chancer.word(syllables=3)))
}


#' Get a random latitude
#'
#' @export
#' @examples
#' chancer.latitude()
chancer.latitude <- function(){
  return(chancer.floating(-90, 90))
}


#' Get a random longitude
#'
#' @export
#' @examples
#' chancer.longitude()
chancer.longitude <- function(){
  return(chancer.floating(-180, 180))
}


#' Get random coordinates
#'
#' @param format either 'vector' or 'string'
#' @export
#' @examples
#' chancer.coordinates()
#' chancer.coordinates(format='string')
chancer.coordinates <- function(format='vector'){
  
  latitude <- chancer.latitude()
  
  longitude <- chancer.longitude()
  
  if(format=='vector'){
    coords <- c(latitude=latitude, longitude=longitude)
  }else if(format=='string'){
    coords <- paste0(latitude, ', ', longitude)
  }
  
  return(coords)
}

#' Get a random depth (are always negative)
#'
#' @param min minimum value, defaults to -2550 (depth of the Mariana Trench)
#' @export
#' @examples
#' chancer.depth()
#' chancer.depth(min=-50)
chancer.depth <- function(min=-2250){
  return(chancer.floating(min=min, max=0))
}


#' Get a random geohash
#'
#' @param len numbers of characters of accuracy (defaults to 7)
#' @export
#' @examples
#' chancer.geohash()
#' chancer.geohash(len=3)
chancer.geohash <- function(len=7){
  return(chancer.string(user="0123456789bcdefghjkmnpqrstuvwxyz", len=len))
}

