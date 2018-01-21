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

encryption_oracle <- function(code){
  code <- "YELLOW SUBMARIN"
  key <- bin_hex(hexl_bin(random_key()))
  before <- hexl_bin(rand_bytes(randbetween(5,10)))
  after <- hexl_bin(rand_bytes(randbetween(5,10)))
  code <- rbind(before, plaintext_bin(code), after)
  length <- length(code)/8
  good_length <- 16*(length %/% 16 + 1)
  padded_code <- bin_hexl(pksc(bin_hexl(code), good_length, bin = 1))
  choose <- randbetween(1,2)
  if (choose == 1){
    r <- ecbEncrypt(padded_code, key = key)
  }
  if (choose == 2){
    
  }
  return(r)
}

encryption_oracle("YELLOW SUBMARIN")

cbcEncrypt <- function (hexl, key, iv){
  plaintext_result <- list()
  length <- length(hexl)/16
  plaintext_result[[1]] <- bin_plaintext(bin_xor((plaintext_bin(ecbDecrypt(hexl[1:16], key))),iv))
  
  #implement CBC mode encryption
  
  for(i in 2:length){
    plaintext_result[[i]] <- bin_plaintext(bin_xor((hexl_bin(ecbDecrypt(hexl[(1 + 16 * i):(16 + 16 * i)], key))),hexl_bin(hexl[(1 + 16 * (i-1)):(16 + 16 * (i-1))])))
  }
  
  #collect list into one string
  
  final <- plaintext_result[[1]]
  for (i in 2:(length(plaintext_result))){
    final <- paste0(final, plaintext_result[[i]])
  }
  return(final)
}


