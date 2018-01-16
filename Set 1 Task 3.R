input <- "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

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
  sol_matrix <- matrix(0, ncol = ((str_length(input))/2), nrow = 16*16)
  common_letters <- c("45","54","4f","41","53","49","4e","61","65","69","6e","6f","74","20")
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
    solutions[k] <- bin_hex(hex_xor(input,keys[k]))
    score[k] <- 0
    for (l in 1:((str_length(input))/2)){
      sol_matrix[k,l] <- substring(solutions[k],(l*2-1),(l*2))
      if (sol_matrix[k,l] %in% common_letters) score[k] <- score[k] + 1
    }
  }
  bin <- hex_xor(input,keys[which.is.max(score)])
  final <- bin_plaintext(bin)
  print(bin_plaintext(hex_bin(keys[which.is.max(score)])))
  return(final)
}

decrypt_single_char_xor(input)


