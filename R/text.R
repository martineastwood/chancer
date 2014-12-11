#' Creates a random syllable either 3 or 4 letters long
#'
#' @export
#' @examples
#' chancer.syllable()
#' 
chancer.syllable <- function(){
  len <- chancer.integer(2, 3)
  consonants <- 'bcdfghjklmnprstvwz' # consonants except hard to speak ones
  vowels <- 'aeiou' # vowels
  all <- paste0(consonants, vowels, collapse='')
  text <- ''
  chr <- ''
  
  for(i in 0:len) {
    if(i == 0) {
      # First character can be anything
      chr <- chancer.character(user=all)
    }else if(grepl(chr, vowels)) {
      # Last character was a vowel, now we want a consonant
      chr <- chancer.character(user=consonants)
    }else{
      # Last character was a consonant, now we want a vowel
      chr <- chancer.character(user=vowels)
    }
    text <- c(text, chr)
  }
  
  text <- paste0(text, collapse='')
  
  return(text)
}

#' Get a random sentence
#'
#' @param words optional to set maximum words in sentence otherwise defaults to 
#' random length between 12 - 18 words
#' @export
#' @examples
#' chancer.sentence()
#' chancer.sentence(words=4)
chancer.sentence <- function(words=NULL){
  
  if(is.null(words)){
    len <- chancer.integer(12, 18)
  }else{
    len <- words
  }
  
  words <- sapply(1:len, function(x){
    chancer.word()
  })
  
  words <- paste(words, collapse=' ')
  
  words <- paste(c(words, '.'), collapse='')
  
  words <- chancer.capitalise(words)
  
  return(words)  
}

#' Get a random word
#'
#' @param len optional maximum length of string to return. Note that only
#' one of len and syllables can be used
#' @param syllables optional maximum number of syllables to use. Note that only
#' one of len and syllables can be used
#' @export
#' @examples
#' chancer.word()
#' chancer.string(len=10, case='lower', pool='alpha')
#' chancer.string(len=10, pool='symbol')
#' 
chancer.word <- function(syllables=NULL, len=NULL){
  
  if(!is.null(syllables) && !is.null(len)){
    stop('Only one of syllables and len can be used')
  }
  
  if(is.null(syllables)){
    sylls <- chancer.integer(1, 3)
  }else{
    sylls <- syllables
  }
  
  text <- ''
  
  if(is.null(len)){
    text <- sapply(1:sylls, function(x){
      chancer.syllable()
    })  
    text <- paste0(text, collapse='')
  }else{
    while(nchar(text) < len){
      text <- paste0(text, chancer.syllable(), collapse='')
    }
    text <- substr(text, 1, len)
  }
  
  return(text)  
}

#' Get a random sentence
#'
#' @param sentences optional to set maximum sentences in paragraph otherwise defaults to 
#' random length between 3 - 7 sentences
#' @export
#' @examples
#' chancer.paragraph()
#' chancer.paragraph(sentences=4)
chancer.paragraph <- function(sentences=NULL){
  
  if(is.null(sentences)){
    len <- chancer.integer(3, 7)
  }else{
    len <- sentences
  }
  
  words <- sapply(1:len, function(x){
    chancer.sentence()
  })
  
  words <- paste(words, collapse=' ')
  
  return(words)  
}

