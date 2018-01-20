library(stringr)

pksc <- function(text, length, bin = 0){
  text <- "YELLOW SUBMARINE"
  length <- 20
  l <- str_length(text)
  pad <- length - l
  r <- plaintext_bin(text)
  m <- int_bin(pad)
  for(i in 1:pad){
    r <- rbind(r, m)
  }
  if (bin == 0){ 
    return(bin_plaintext(r))
  }else if(bin != 0){
    return(r)
  }
}

pksc("YELLOW SUBMARINE", 20)