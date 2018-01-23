library(stringr)

input <- "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexString_raw <- function(input){
  in_raw <- raw(str_length(input)/2)
  for(i in 1:(length(in_raw))){
    in_raw[i] <- as.raw(as.hexmode((substring(input,(i*2-1),(i*2)))))
  }
  return(in_raw)
}
 
out <- base64enc::base64encode(hexString_raw(input))



xor()