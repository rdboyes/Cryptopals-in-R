input1 <- "1c0111001f010100061a024b53535009181c"
input2 <- "686974207468652062756c6c277320657965"

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

bin <- hex_xor(input1,input2)
bin_hex(bin)
