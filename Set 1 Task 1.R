hex_to_base64 <- function(input){
  output <- ""
  base64values = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  int_array= array(data = NA)
  hex_as_num = array(data = NA)
  encrypt_array = array(data = NA)
  for(h in 1:(str_length(base64values))){
    encrypt_array[h] <- substring(base64values, h, h)
  }
  for(i in 1:(str_length(input))){
    int_array[i] <- as.numeric(as.hexmode(substring(input, i, i)))
  }
  padding <- length(int_array)%%3
  if (padding > 0){
    for (l in 1:padding){
      int_array[length(int_array+1)] <- 0
    }
  }
  for (j in 1:(length(int_array)/3)){
    hex_as_num[j*2-1] <- int_array[j*3-2]*4 + int_array[j*3-1]%/%4
    hex_as_num[j*2] <- int_array[j*3-1]%%4*16 + int_array[j*3]
  }
  for(k in 1:length(hex_as_num)){
    output <- paste0(output,encrypt_array[hex_as_num[k]+1])
  }
  return(output)
}

hex_to_base64("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

