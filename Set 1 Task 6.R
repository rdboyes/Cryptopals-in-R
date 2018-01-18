#import encrypted file

library(readr)
library(openssl)
library(stringr)

b64_text <- read_csv("Downloads/b64_text.txt", col_names = FALSE)
d <- as.matrix(b64_text)
hex <- base64_decode(d)

#edit distance function

edit_distance <- function(input1, input2){
  length <- (length(input1))/4
  distance <- 0
  for(i in 1:length){
    for(j in 1:4){
      if(input1[i,j] != input2[i,j]){
        distance <- distance + 1
      }
    }
  }
  return(distance)
}

#test to make sure the edit distance function works

input1 <- "this is a test"
input2 <- "wokka wokka!!!"
bin1 <- plaintext_bin(input1)
bin2 <- plaintext_bin(input2)

edit_distance(bin1, bin2) #37

#make blank array to hold normalized differences

distances <- array(data = NA)

#compute normalized edit difference for 1st and 2nd block of length = key length

bin_encoded_whole <- array()
bin_encoded_whole <- hex_bin(hex[1])
for (i in 2:2876){
  m <- hex_bin(hex[i])
  bin_encoded_whole <- rbind(bin_encoded_whole,m)
}

for(j in 2:40){
  keyblock1 <- bin_encoded_whole[1:(2*j),1:4]
  keyblock2 <- bin_encoded_whole[(2*j+1):(4*j),1:4]
  keyblock3 <- bin_encoded_whole[(4*j+1):(6*j),1:4]
  keyblock4 <- bin_encoded_whole[(6*j+1):(8*j),1:4]
  distances[j] <- (edit_distance(keyblock1,keyblock2) + edit_distance(keyblock1,keyblock3) + edit_distance(keyblock1,keyblock4) + edit_distance(keyblock2,keyblock4) + edit_distance(keyblock3,keyblock4) + edit_distance(keyblock2,keyblock3))/j
}

#which key length has the lowest normalized edit distance? 

real_key_length <- which.min(distances) #29

#Find the key character for each block

single_key_length <- (length(bin_encoded)/8)%/%real_key_length

#convert the encrypted file to list of n binary matrices

bin_encoded <- list()
for (a in 1:real_key_length){
  bin_encoded[[a]] <- hex_bin(hex[a])
  max_i <- (2877 - a)%/%(real_key_length)
  for (i in 1:max_i){
    m <- hex_bin(hex[(a+i*real_key_length)])
    bin_encoded[[a]] <- rbind(bin_encoded[[a]],m)
  }
}

#function that decrypts single char xor

decrypt_single_char_xor <- function(hex){
  keys <- array()
  hexValues = "0123456789abcdef"
  encrypt_array <- array()
  solutions <- array(data = NA)
  sol_matrix <- matrix(0, ncol = ((str_length(hex))/2), nrow = 16*16)
  common_letters <- c("65","74","61","69","6f","6e","20","72","68","72","64","6c","75") #etaion shrdlu
  letters <- c("61","62","63","64","65","66","67","68","69","6a","6b","6c","6d","6e","6f","70","71","72","73","74","75","76","77","78","79","7a")
  not_letters <- c("01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19")
  symbols <- c("21","22","23","24","25","26","27","29","2a","2b","2c","2e","2f","3a","3b","3c","3d","3e","3f","40","5b","5c","5d","5e","5f","60")
  numbers <- c("30","31","32","33","34","35","36","37","38","39")
  score <- array(data = NA)
  for(h in 1:(str_length(hexValues))){
    encrypt_array[h] <- substring(hexValues, h, h)
  }
  for (i in 1:16){
    for(j in 1:16){
      keys[(i-1)*16 + j] <- paste0(encrypt_array[i],encrypt_array[j])
    }
  }
  for(k in 1:length(keys)){
    solutions[k] <- bin_hex(hex_xor(hex,keys[k]))
    score[k] <- 0
    for (l in 1:((str_length(hex))/2)){
      sol_matrix[k,l] <- substring(solutions[k],(l*2-1),(l*2))
      if (sol_matrix[k,l] %in% common_letters) score[k] <- score[k] + 10
      if (sol_matrix[k,l] %in% letters) score[k] <- score[k] + 2
      if (sol_matrix[k,l] %in% numbers) score[k] <- score[k] + 2
      if (sol_matrix[k,l] %in% symbols) score[k] <- score[k] - 20
      if (sol_matrix[k,l] %in% not_letters) score[k] <- score[k] - 50 
    }
  }
  bin <- hex_xor(hex,keys[which.max(score)])
  return(bin)
}

#Reorganize all the letters

realtext <- list()
for (j in 1:real_key_length){
  m <- matrix()
  m <- bin_encoded[[j]]
  realtext[[j]] <- decrypt_single_char_xor(bin_hex(m))  
}

length <- length(realtext[[1]])/8
textmatrix <- matrix(0, nrow = 2, ncol = 4)
for(l in 1:(length-1)){
  for (k in 1:(real_key_length)){
    m <- realtext[[k]][(l*2-1):(l*2),1:4]
    textmatrix <- rbind(textmatrix,m)
  }
}

#Output decrypted text

bin_plaintext(textmatrix)

