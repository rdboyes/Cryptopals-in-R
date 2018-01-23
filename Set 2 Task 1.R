library(stringr)

pksc <- function(text, length, raw = 1){
  if(!is.raw(text)){
    text <- charToRaw(text)
  }
  l <- length(text)
  pad <- length - l
  r <- text
  m <- as.raw(pad)
  for(i in 1:pad){
    r <- c(r, m)
  }
  if (raw == 0){ 
    return(rawToChar(r))
  }else if(raw != 0){
    return(r)
  }
}

pksc("YELLOW SUBMARINE", 20, raw = 0)

