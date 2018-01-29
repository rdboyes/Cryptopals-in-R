#Challenge 16: CBC Bitflipping attacks

library(openssl)

random_key <- function(){
  a <- rand_bytes(16)
  return(a)
}

pksc <- function(text, raw = 1){
  if(!is.raw(text)){
    text <- charToRaw(text)
  }
  l <- length(text)
  length <- (1 + l%/%16)*16
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

consistent_key <- random_key()
iv <- random_key()

create_user_data <- function(userdata){
  userdata <- gsub('=|;','',userdata)
  input <- charToRaw(userdata)
  pre <- charToRaw("comment1=cooking%20MCs;userdata=")
  post <- charToRaw(";comment2=%20like%20a%20pound%20of%20bacon")
  output <- c(pre,input,post)
  padded <- pksc(output)
  enc_output <- cbcEncrypt(padded,consistent_key,iv)
  return(enc_output)
}

check_admin <- function(raw){
  input <- cbcDecrypt(raw,consistent_key,iv)
  plaintext <- rawToChar(input)
  return(grepl(";admin=true;",plaintext))
}

check_admin(create_user_data(";admin=true;")) # FALSE



