hex_bin <- function(hex){
  h <- array()
  length <- str_length(hex)
  for(i in 1:length){
    h[i] <- as.hexmode(substring(hex, i, i))
  }
  m <- matrix(0, nrow = length, ncol = 4)
  for (k in 1:length){
    m[k,1] <- h[k]%/%8
    m[k,2] <- (h[k] - m[k,1]*8)%/%4
    m[k,3] <- (h[k] - m[k,1]*8 - m[k,2]*4)%/%2
    m[k,4] <- (h[k] - m[k,1]*8 - m[k,2]*4 - m[k,3]*2)
  }
  return(m)
}

bin_plaintext <- function(bin){
  decode <- array()
  length <- length(bin)
  output <- ""
  for(i in 1:(length/8)){
    x <- bin[i*2-1,1]*128 + bin[i*2-1,2]*64 + bin[i*2-1,3]*32 + bin[i*2-1,4]*16 + bin[i*2,1]*8 + bin[i*2,2]*4 + bin[i*2,3]*2 + bin[i*2,4]
    decode[i] <- rawToChar(as.raw(x),10L)
    output <- paste0(output,decode[i])
  }
  return(output)
}

decrypt_single_char_xor <- function(hex){
  keys <- array()
  hexValues = "0123456789abcdef"
  encrypt_array <- array()
  solutions <- array(data = NA)
  sol_matrix <- matrix(0, ncol = ((str_length(hex))/2), nrow = 16*16)
  common_letters <- c("65","74","61","69","6f","6e","20","72","68","72","64","6c","75") #etaion shrdlu
  not_letters <- c("01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19")
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
      if (sol_matrix[k,l] %in% common_letters) score[k] <- score[k] + 1
      if (sol_matrix[k,l] %in% not_letters) score[k] <- score[k] - 1 
    }
  }
  bin <- hex_xor(input,keys[which.is.max(score)])
  final <- bin_plaintext(bin)
  print(bin_plaintext(hex_bin(keys[which.is.max(score)])))
  return(final)
}

bin_hex <- function(bin){
  hexValues = "0123456789abcdef"
  encrypt_array <- array()
  out_array <- array()
  length <- length(bin)/4
  for(h in 1:(str_length(hexValues))){
    encrypt_array[h] <- substring(hexValues, h, h)
  }
  output <- ""
  for(j in 1:length){
    out_array[j] <- 8*bin[j,1] + 4*bin[j,2] + 2*bin[j,3] + bin[j,4] 
    output <- paste0(output,encrypt_array[out_array[j]+1])
  }
  return(output)
}

hex_xor <- function(input1, input2){
  length <- str_length(input1)
  length2 <- str_length(input2)
  x <- input2
  while (length > length2){
    x <- paste0(x,input2)
    length2 <- str_length(x)
  } 
  bin1 <- hex_bin(input1)
  bin2 <- hex_bin(x)
  m <- matrix(0, ncol = 4, nrow = length)
  for(i in 1:length){
    for(j in 1:4){
      m[i,j] <- (bin1[i,j] + bin2[i,j])%%2
    }
  }
  return(m)
}

bin_b64 <- function(bin){
  base64values = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  encrypt_array <- array()
  out_array <- array()
  length <- length(bin)/6
  m <- matrix(0,nrow = length, ncol = 6)
  for(h in 1:(str_length(base64values))){
    encrypt_array[h] <- substring(base64values, h, h)
  }
  for(i in 1:(length/2)){
    m[i*2-1,1] <- bin[i*3-2,1]
    m[i*2-1,2] <- bin[i*3-2,2]
    m[i*2-1,3] <- bin[i*3-2,3]
    m[i*2-1,4] <- bin[i*3-2,4]
    m[i*2-1,5] <- bin[i*3-1,1]
    m[i*2-1,6] <- bin[i*3-1,2]
    m[i*2,1] <- bin[i*3-1,3]
    m[i*2,2] <- bin[i*3-1,4]
    m[i*2,3] <- bin[i*3,1]
    m[i*2,4] <- bin[i*3,2]
    m[i*2,5] <- bin[i*3,3]
    m[i*2,6] <- bin[i*3,4]
  }
  output <- ""
  for(j in 1:length){
    out_array[j] <- 32*m[j,1] + 16*m[j,2] + 8*m[j,3] + 4*m[j,4] + 2*m[j,5] + m[j,6] 
    output <- paste0(output,encrypt_array[out_array[j]+1])
  }
  return(output)
}

plaintext_bin <- function(text){
  length <- str_length(text)
  pt_array <- array()
  text_array <- array()
  num_array <- array(data = 0)
  for (i in 1:length){
    text_array[i] <- substring(text, i, i)
    num_array[i] <- as.numeric(charToRaw(text_array[i]))
  }
  m <- matrix(0, nrow = length*2, ncol = 4)
  for (k in 1:length){
    m[k*2-1,1] <- num_array[k]%/%128
    m[k*2-1,2] <- (num_array[k] - m[k*2-1,1]*128)%/%64
    m[k*2-1,3] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64)%/%32
    m[k*2-1,4] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32)%/%16
    m[k*2,1] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16)%/%8
    m[k*2,2] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8)%/%4
    m[k*2,3] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4)%/%2
    m[k*2,4] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4 - m[k*2,3]*2)
  }
  return(m)
}

int_bin <- function(num){
  k <- 1
  m <- matrix(0, nrow = 2, ncol = 4)
  m[k*2-1,1] <- num%/%128
  m[k*2-1,2] <- (num - m[k*2-1,1]*128)%/%64
  m[k*2-1,3] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64)%/%32
  m[k*2-1,4] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32)%/%16
  m[k*2,1] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16)%/%8
  m[k*2,2] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8)%/%4
  m[k*2,3] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4)%/%2
  m[k*2,4] <- (num - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4 - m[k*2,3]*2)
  return(m)
}