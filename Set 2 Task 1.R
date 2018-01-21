library(stringr)

pksc <- function(text, length, bin = 0){
  if(!is.raw(text)){
    text <- as.raw(text)
  }
  l <- length(text)
  pad <- length - l
  r <- hexl_bin(text)
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

pksc("YELLOW SUBMARIN", 16)