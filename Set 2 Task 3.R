#An ECB/CBC detection oracle
#Now that you have ECB and CBC working:
#Write a function to generate a random AES key; that's just 16 random bytes.
#Write a function that encrypts data under an unknown key --- that is, a function that generates a random key and encrypts under it.
#The function should look like:
#encryption_oracle(your-input)
#=> [MEANINGLESS JIBBER JABBER]
#Under the hood, have the function append 5-10 bytes (count chosen randomly) before the plaintext and 5-10 bytes after the plaintext.
#Now, have the function choose to encrypt under ECB 1/2 the time, and under CBC the other half (just use random IVs each time for CBC). Use rand(2) to decide which to use.
#Detect the block cipher mode the function is using each time. You should end up with a piece of code that, pointed at a block box that might be encrypting ECB or CBC, tells you which one is happening.

library(openssl)

random_key <- function(){
  a <- rand_bytes(16)
  return(a)
}

randbetween <- function(min, max){
  return(floor(runif(1, min = min, max = (max+1))))
}
  
bin_hexl <- function(bin){
  l <- length(bin)/8
  x <- 0
  raw <- as.raw(0)
  for(i in 1:l){
    x[i] <- (bin[i*2-1,1] * 128 + bin[i*2-1,2] * 64 + bin[i*2-1,3] * 32 + bin[i*2-1,4] * 16 + bin[i*2,1] * 8 + bin[i*2,2] * 4 + bin[i*2,3] * 2 + bin[i*2,4]) 
    raw[i] <- as.raw(x[i])
  }
  return(raw)
}

cbcEncrypt <- function (hexl, key, iv){
  plaintext_result <- list()
  length <- length(hexl)/16
  if(!is.raw(key)){
    key <- charToRaw(key)
  }
  if(!is.raw(hexl)){
    text <- charToRaw(hexl)
  }
  plaintext_result[[1]] <- ecbEncrypt(xor(hexl[1:16],key), iv)
  
  #implement CBC mode encryption
  
  for(i in 2:length){
    plaintext_result[[i]] <- ecbEncrypt(xor(plaintext_result[[(i - 1)]], hexl[(i * 16):(i * 16 + 15)]),key)
  }
  
  #collect list into one vector
  
  final <- plaintext_result[[1]]
  for (i in 2:(length(plaintext_result))){
    final <- c(final, plaintext_result[[i]])
  }
  return(final)
}

encryption_oracle <- function(code){
  if(!is.raw(code)){
    code <- charToRaw(code)
  }
  key <- random_key()
  before <- rand_bytes(randbetween(5,10))
  after <- rand_bytes(randbetween(5,10))
  code <- c(before, code, after)
  length <- length(code)
  good_length <- 16*(length %/% 16 + 1)
  padded_code <- pksc(code, good_length)
  choose <- randbetween(1,2)
  if (choose == 1){
    r <- ecbEncrypt(padded_code, key = key)
    print("ECB")
    return(r)
  }
  if (choose == 2){
    r <- cbcEncrypt(padded_code, key = key, iv = random_key())
    print("CBC")
    return(r)
  }
}

is_ecb(encryption_oracle("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))


