#import encrypted file

library(readr)
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

#convert the encrypted file to binary

bin_encoded <- hex_bin(hex[1])
for (i in 2:2876){
  m <- hex_bin(hex[i])
  bin_encoded <- rbind(bin_encoded,m)
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

for(j in 2:40){
  keyblock1 <- bin_encoded[1:(2*j),1:4]
  keyblock2 <- bin_encoded[(2*j+1):(4*j),1:4]
  distances[j] <- (edit_distance(keyblock1,keyblock2)/j)
}

#which key length has the lowest normalized edit distance? 

real_key_length <- which.min(distances)

#Find the key character for each block

single_key_length <- (length(bin_encoded)/8)%/%real_key_length

piece <- bin_encoded[1:2,1:4]
for(i in 2:single_key_length){
  x <- (1+2*(i-1)*real_key_length)
  piece <- rbind(piece,bin_encoded[x:(x+1),1:4])
}

p1 <- bin_hex(piece)

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
  final <- array()
  for (l in 1:10){
    bin <- hex_xor(input,keys[index.of(maxN(score, N = l))])
    final[i] <- bin_plaintext(bin)
  }
  return(final)
}

maxN <- function(x, N=3){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}


decrypt_single_char_xor(p1)
