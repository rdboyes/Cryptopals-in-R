#Create two functions - one to encrypt with ECB and the other to decrypt with ECB. Test to make sure they work

key <- "YELLOW SUBMARINE"
testmsg <- "TESTING THE CODE"

ecbDecrypt <- function(text, key){
  library(digest)
  aes <- AES(mode = "ECB",key = charToRaw(key))
  result <- aes$decrypt(text, raw = TRUE)
  return(result)
}

ecbEncrypt <- function(text, key){
  library(digest)
  aes <- AES(mode = "ECB",key = charToRaw(key))
  result <- aes$encrypt(text)
  return(result)
}

ecbDecrypt(ecbEncrypt(testmsg, key),key) #> TESTING THE CODE

#Read file to be decrypted with CBC

library(readr)
library(openssl)
b64_text <- as.matrix(read_csv("/Users/Randy/Downloads/10.txt", col_names = FALSE))
msg <- base64_decode(as.matrix(b64_text))

#function that converts vector of hex bytes to n by 4 binary matrix
hexl_bin <- function(hexl){
  bin_encoded_whole <- array()
  bin_encoded_whole <- hex_bin(hexl[1])
  for (i in 2:length(hexl)){
    m <- hex_bin(hexl[i])
    bin_encoded_whole <- rbind(bin_encoded_whole,m)
  }
  return(bin_encoded_whole)
}

#Create IV and XOR it with the first 16 bytes of the message
iv <- "00"
for(i in 1:16){
  iv[i] <- "00"
}

cbcDecrypt <- function (hexl, key, iv){
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

cbcDecrypt(msg,"YELLOW SUBMARINE",iv)










