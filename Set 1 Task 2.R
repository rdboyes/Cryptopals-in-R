#Hex XOR function

input1 <- "1c0111001f010100061a024b53535009181c"
input2 <- "686974207468652062756c6c277320657965"

hex_XOR <- function(input1, input2){
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
for(i in 1:(str_length(input2))){
  int_array2[i] <- as.numeric(as.hexmode(substring(input2, i, i)))
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

hex_XOR(input1,input2)
