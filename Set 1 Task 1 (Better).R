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

input <- "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

start_time <- Sys.time()
bin <- hex_bin(input)
bin_b64(bin)
end_time <- Sys.time()

end_time - start_time #0.0035 s

