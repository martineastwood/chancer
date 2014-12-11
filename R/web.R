#' Get a random colour
#'
#' @param format can be one of 'hex', 'rgb', 'shorthex'
#' @param greyscale TRUE or FALSE whether to restrict colour to greyscale
#' @export
#' @examples
#' chancer.colour()
#' chancer.colour(format='rgb')
#' chancer.colour(format='rgb', greyscale=TRUE)
chancer.colour <- function(format='hex', greyscale=FALSE){
  
  if(format == 'hex'){
    if(greyscale){
      tmp <- chancer.hash(2)
      colour <- paste0('#', tmp, tmp, tmp)
    }else{
      colour <- paste0('#', chancer.hash(6))
    }
  }else if(format == 'rgb'){
    if(greyscale){
      tmp <- chancer.natural(max=255)
      colour <- paste0('rgb(', tmp, ',', tmp, ',', tmp, ')')
    }else{
      colour <- paste0('rgb(', chancer.natural(max=255), ',', 
                       chancer.natural(max=255), ',', 
                       chancer.natural(max=255), ')')
    }
  }else if(format=='shorthex'){
    if(greyscale){
      colour <- paste0('#', chancer.hash(1))
    }else{
      colour <- paste0('#', chancer.hash(3))
    }
  }else{
    stop("format must be one of hex, rgb or shorthex")
  }
  
  return(colour)
}

#' Get a random domain name
#'
#' @param tld if NULL the tld is random or specifically set 
#' @export
#' @examples
#' chancer.domain()
#' chancer.domain(tld='ie')
chancer.domain <- function(tld=NULL){
  
  domain <- chancer.word()
  
  if(is.null(tld)){
    tld <- chancer.tld()
  }
  
  domain <- paste0(domain, '.', tld, collapse='')
  
  return(domain)
}

#' Get a random email address
#'
#' @param domain if NULL the domain is random or can be specifically set 
#' @export
#' @examples
#' chancer.email()
#' chancer.email(domain='rmail.com')
chancer.email <- function(domain=NULL){
  
  email <- chancer.word()
  
  if(is.null(domain)){
    domain <- chancer.domain()
  }
  
  email <- paste0(email, '@', domain, collapse='')
  
  return(email)
}

#' Get a random facebook id (fbid)
#'
#' @export
#' @examples
#' chancer.facebook()
chancer.facebook <- function(){
  
  fbid <- paste0("10000", as.character(chancer.natural(max=100000000)))
  
  return(fbid)
}

#' Get a random google analytics id
#'
#' @export
#' @examples
#' chancer.google()
chancer.google <- function(){
  
  account <- paste0(sapply(1:6, chancer.natural, max=9), collapse='')
  
  property <- paste0(sapply(1:2, chancer.natural, max=9), collapse='')
  
  id <- paste0('UA-', account, '-', property)
  
  return(id)
}

#' Gets a random hashtag
#'
#' @export
#' @examples
#' chancer.hashtag()
chancer.hashtag <- function(){
  
  tag <- paste0('#', chancer.word())
  
  return(tag)
}

#' Get a random ip address
#'
#' @export
#' @examples
#' chancer.ip()
chancer.ip <- function(){
  
  ip <- paste0(chancer.natural(max=255),
               '.',
               chancer.natural(max=255),
               '.',
               chancer.natural(max=255),
               '.',
               chancer.natural(max=255), collapse='')
  
  return(ip)
}

#' Get a random ipv6 address
#'
#' @export
#' @examples
#' chancer.ipv6()
chancer.ipv6 <- function(){
  
  ip <- sapply(1:8, function(x){
    chancer.hash(len=4, case='lower')
  })
  
  ip <- paste(ip, collapse=':')
  
  return(ip)
}

#' Get a random klout score
#'
#' @export
#' @examples
#' chancer.klout()
chancer.klout <- function(){  
  return(chancer.natural(min=1, max=99))
}

#' Get a random top level domain
#'
#' @export
#' @examples
#' chancer.tld()
chancer.tld <- function(){
  return(chancer.pick(TLDS))
}

#' Get a random twitter handle
#'
#' @export
#' @examples
#' chancer.twitter()
chancer.twitter <- function(){
  return(paste0('@', chancer.word()))
}

