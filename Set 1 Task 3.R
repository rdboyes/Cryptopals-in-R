make_array <- function(text){
  text_array <- array(data = NA)
  for(h in 1:str_length(text)){
    text_array[h] <- substring(text, h, h)
  }
  return(text_array)
}

score_text <- function(text){
  letterfreq <- "8134922670142782066931202081349226701427820669312020"
  alphabet <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  alpha_array <- make_array(alphabet)
  freq_array <- make_array(letterfreq)
  text_array <- make_array(text)
  score <- 0
  for(i in 1:length(text_array)){
    for(j in 1:52){
      if (text_array[i] == alpha_array[j]){
        score <- score + as.integer(freq_array[j])
      } 
    }
  }
  return(score)
}

hex_to_ASCII <- function(text){
  h <- sapply(seq(1, nchar(text), by=2), function(x) substr(text, x, x+1))
  rawToChar(as.raw(strtoi(h, 16L)))
}

hex_XOR_1 <- function(input1, cipher){
  hexValues <- "123456789abcdef0"
  hexValues_array <- array(data = NA)
  int_array1 <- array(data = NA)
  bin_array1 <- array(data = NA)
  int_array2 <- array(data = NA)
  bin_array2 <- array(data = NA)
  out_array <- array(data = NA)
  out_array_bin <- array(data = NA)
  output <- ""
  for(i in 1:(str_length(input1))){
    int_array1[i] <- as.numeric(as.hexmode(substring(input1, i, i)))
    b <- 1
    n <- int_array1[i]
    while(b < 5) {
      bin_array1[1+i*4 - b] <- n%%2
      n <- n%/%2
      b <- b + 1
    }
  }
  cipher_array <- make_array(cipher)
  for(i in 1:(str_length(input1))){
    int_array2[i] <- as.numeric(cipher_array[i%%length(cipher_array)+1])
    b <- 1
    n <- int_array2[i]
    while(b < 5) {
      bin_array2[1+i*4 - b] <- n%%2
      n <- n%/%2
      b <- b + 1
    }
  }
  for(k in 1:length(bin_array1)){
    out_array_bin[k] <- (bin_array1[k] + bin_array2[k])%%2
  }
  for(n in 1:(length(out_array_bin)/4)){
    out_array[n] <- 8*out_array_bin[n*4-3] + 4*out_array_bin[n*4-2] + 2*out_array_bin[n*4-1] + out_array_bin[n*4]   
  }
  for(l in 1:str_length(hexValues)){
    hexValues_array[l] <- substring(hexValues,l,l)
  }
  for(m in 1:length(out_array)){
    output <- paste0(output,hexValues_array[out_array[m]])
  }
  return(output)
}

input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
highscore <- 0
decrypted <- ""
for(m in 1:127){
  attempt <- hex_XOR_1(input,m)
  score <- score_text(hex_to_ASCII(attempt))
  if (score > highscore){
    highscore <- score
    decrypted <- attempt
  }
}
decrypted
  
score_text("hllccadeas")

