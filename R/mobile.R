#' Get a random Android GCM registration ID
#'
#' @export
#' @examples
#' chancer.android_id()
chancer.android_id <- function(){
  
  pool <- "0123456789abcefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"
  
  id <- chancer.string(178, user=pool)
  
  id <- paste0("APA91", id, collapse='')
  
  return(id)
}

#' Get a random Apple Push Token
#'
#' @export
#' @examples
#' chancer.apple_token()
chancer.apple_token <- function(){
  
  pool <- "abcdef1234567890"
  
  id <- chancer.string(64, user=pool)
  
  return(id)
}
 
#' Get a random BlackBerry pin
#'
#' @export
#' @examples
#' chancer.bb_pin()
chancer.bb_pin <- function(){
  return(chancer.hash(8))
}

#' Get a random Windows Phone 7 ANID
#'
#' @export
#' @examples
#' chancer.wp7_anid()
chancer.wp7_anid <- function(){
  anid <- paste0('A=', chance)
  return(chancer.hash(8))
}


#return 'A=' + this.guid().replace(/-/g, '').toUpperCase() + '&E=' + this.hash({ length:3 }) + '&W=' + this.integer({ min:0, max:9 });