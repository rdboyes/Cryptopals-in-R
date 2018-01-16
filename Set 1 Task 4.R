library(readr)
xor_text <- read_csv("Downloads/xor text.txt", col_names = FALSE)
d <- as.matrix(xor_text)
solutions4 <- array()

for(i in 1:327){
  solutions4[i] <- decrypt_single_char_xor(d[i])
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
      if (sol_matrix[k,l] %in% common_letters) score[k] <- score[k] + 5
      if (sol_matrix[k,l] %in% not_letters) score[k] <- score[k] - 2 
    }
  }
  bin <- hex_xor(input,keys[which.is.max(score)])
  final <- bin_plaintext(bin)
  print(bin_plaintext(hex_bin(keys[which.is.max(score)])))
  return(final)
}

