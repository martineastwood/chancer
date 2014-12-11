#' Get a random age
#'
#' @param type if NULL defaults to age between 1-100 otherwise can be one of
#' 'child', 'teen', 'adult', 'senior' to pick a pre-defined age range
#' @export
#' @examples
#' chancer.age()
#' chancer.age(type='child')
#' chancer.age(type='senior')
chancer.age <- function(type=NULL){
  
  if(is.null(type)){
    age <- chancer.integer(1, 100)
  }else if(type=='child'){
    age <- chancer.integer(1, 12)
  }else if(type=='teen'){
    age <- chancer.integer(13, 19)
  }else if(type=='adult'){
    age <- chancer.integer(18, 65)
  }else if(type=='senior'){
    age <- chancer.integer(65, 100)
  }else{
    stop("type must be one of NULL, 'child', 'teen', 'adult', 'senior'")
  }
  
  return(age)  
}

#' Get a random birthday
#'
#' @param type eiter 'POSIXlt' to return a POSIXlt object or 'string' to return 
#' date as a string
#' @export
#' @examples
#' chancer.birthday()
#' chancer.birthday(type='string')
chancer.birthday <- function(type='POSIXlt'){
  
  bday <- as.POSIXlt(Sys.time())
  
  age <- chancer.age()
  
  bday$year <- bday$year - age
  
  if(type=='POSIXlt'){
    # do nothing
  }else if(type=='string'){
    bday <- as.character(bday, format='%Y-%m-%d %H:%M:%S')
  }else{
    stop("type must be one of NULL, 'child', 'teen', 'adult', 'senior'")
  }
  
  return(bday)  
}

#' Get a random first name
#'
#' @param gender defaults to NULL but can get set to either 'male' or 'female'
#' @export
#' @examples
#' chancer.first()
#' chancer.first(gender='female')
chancer.first <- function(gender=NULL){
  
  if(is.null(gender)){
    pool <- c(FIRSTNAMES$male, FIRSTNAMES$female)
    return(chancer.pick(pool))
  }else if(gender=='male'){
    return(chancer.pick(FIRSTNAMES$male))
  }else if(gender=='female'){
    return(chancer.pick(FIRSTNAMES$female))
  }else{
    stop("gender must be one of NULL, 'male', 'female'")
  }
}

#' Get a random surname name
#'
#' @export
#' @examples
#' chancer.last()
chancer.last <- function(){
  return(chancer.pick(LASTNAMES))
}

#' Get a random name
#'
#' @param initial TRUE or FALSE to include a middle initial
#' @param middle_name TRUE or FALSE to include a middle name
#' @param gender if NULL gender is randomly selected else can be one of
#' 'male' or 'female'
#' @param prefix TRUE or FALSE to include a prefix to the name, e.g Doctore, Mister etc
#' @export
#' @examples
#' chancer.name()
#' chancer.name(initial=TRUE)
#' chancer.name(middle_name=TRUE)
#' chancer.name(middle_name=TRUE, prefix=TRUE)
chancer.name <- function(initial=FALSE, middle_name=FALSE, gender=NULL, prefix=FALSE){
  
  if(initial && middle_name){
    stop('Only one of middle_name and initial can be TRUE')
  }
  
  if(is.null(gender)){
    gender <- chancer.gender()
  }
  
  if(!gender %in% c('male', 'female')){
    stop('Gender must be one of male or female')
  }
  
  first <- chancer.first(gender=gender)
  
  last <- chancer.last()
  
  middle = NULL
  
  if(initial){
    first <- paste(first, chancer.character(case='upper', pool='alpha'))
  }else if(middle_name){
    first <- paste(first, chancer.first(gender=gender))
  }
  
  if(prefix){
    first <- paste(chancer.prefix(gender=gender), first)
  }
  
  full_name <- paste(first, last)
  
  return(full_name)
}

#' Get a random prefix for name, e.g Mister, Doctor etc
#'
#' @param gender one of 'male', 'female', 'all'
#' @param abbreviated boolean whether to abbreviate the prefix or not
#' @export
#' @examples
#' chancer.prefix()
#' chancer.prefix('male', TRUE)
chancer.prefix <- function(gender=NULL, abbreviated=FALSE){
  
  if(is.null(gender)){
    if(abbreviated){
      pool <- unique(c(NAME_PREFIX_MALE$abbreviated, NAME_PREFIX_FEMALE$abbreviated))
    }else{
      pool <- unique(c(NAME_PREFIX_MALE$full, NAME_PREFIX_FEMALE$full))
    }
  }else if(gender == 'male'){
    if(abbreviated){
      pool <- NAME_PREFIX_MALE$abbreviated
    }else{
      pool <- NAME_PREFIX_MALE$full
    }
  }else if(gender == 'female'){
    if(abbreviated){
      pool <- NAME_PREFIX_FEMALE$abbreviated
    }else{
      pool <- NAME_PREFIX_FEMALE$full
    }
  }else{
    stop('gender must be one of male, female, NULL')
  }
  
  prefix <- chancer.pick(pool)
  
  return(prefix)  
}


#' Get a random gender
#'
#' @export
#' @examples
#' chancer.gender()
chancer.gender <- function(type=NULL){
  
  pool <- c('male', 'female')
  
  gender <- chancer.pick(pool)
  
  return(gender)  
}

#' Get a random social security number
#'
#' @param dashes TRUE or FALSE to have the dashes within the social security number
#' @param last_four whether to return just the last four digits or not
#' @export
#' @examples
#' chancer.ssn()
#' chancer.ssn(dashes=FALSE)
#' chancer.ssn(last_four=TRUE)
chancer.ssn <- function(dashes=TRUE, last_four=FALSE){
  
  ssn <- paste0(chancer.integer(0, 9),
                chancer.integer(0, 9),
                chancer.integer(0, 9), 
                '-',
                chancer.integer(0, 9),
                chancer.integer(0, 9),
                '-',
                chancer.integer(0, 9),
                chancer.integer(0, 9),
                chancer.integer(0, 9),
                chancer.integer(0, 9),
                collapse='')
  
  if(!dashes){
    ssn <- gsub("-", "", ssn)
  }
  
  if(last_four){
    ssn <- substr(ssn, nchar(ssn) - 3, nchar(ssn))
  }
  
  return(ssn)
}