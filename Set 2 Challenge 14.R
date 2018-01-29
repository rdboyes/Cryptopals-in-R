library(openssl)

consistent_key <- random_key()
prepend <- rand_bytes(randbetween(1,100))
append <- "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
YnkK"

consistent_encryption_oracle <- function(code = ""){
  if(!is.raw(code)){
    code <- charToRaw(code)
  }
  code <- c(prepend,code,base64_decode(append))
  key <- consistent_key
  length <- length(code)
  good_length <- 16*(length %/% 16 + 1)
  padded_code <- pksc(code, good_length)
  r <- ecbEncrypt(padded_code, key = key)
  return(r)
}

test_string <- ""
found <- 0
i <- 1
while(found != 1){
  test_string <- paste0(test_string,"A")
  consistent_encryption_oracle_2(test_string)
  if (is_ecb(consistent_encryption_oracle_2(test_string))){
    found <- 1
    oracle_text_raw <- consistent_encryption_oracle_2(test_string)
  }
  print(i)
  i <- i + 1
}
guess_length <- i%/%2

key_length <- function(raw_hex, start_value){
  key_found <- 0
  while(key_found == 0){
    matchCount <- 0
    stringLength <- (length(raw_hex) - start_value)
    for(j in 1:(stringLength)){
      ref <- raw_hex[(j):(j+(start_value - 1))]
      for(k in j:stringLength){
        test <- raw_hex[(k):(k+(start_value - 1))]
        if(isTRUE(all.equal(ref,test))){
          matchCount <- matchCount + 1  
        }
      }
    }
    if (matchCount > stringLength){
      key_found <- 1
    }else{
      start_value <- start_value - 1
    }
  }
  return(start_value)
}

keyl <- key_length(oracle_text_raw, guess_length) # 16

is_ecb(oracle_text_raw) # TRUE

chop_length <- function(raw, keyl){
  i <- 1
  found <- 0
  if (!is.raw(raw)){
    return("Function requires raw vector for input.")
  }
  while(found == 0 && i < (length(raw)/keyl)){
    if(isTRUE(all.equal(raw[(1+(i-1)*keyl):((1+(i-1)*keyl)+(keyl-1))],raw[(1+(i)*keyl):((1+(i)*keyl)+(keyl-1))]))){
      found <- i
    }
    i <- i + 1
  }
  return(found - 1)
}

chop <- chop_length(oracle_text_raw,16)

fixed_oracle <- function(code = ""){
  if(!is.raw(code)){
    code <- charToRaw(code)
  }
  temp_raw <- raw()
  for (i in 1:((guess_length - keyl)*2)){
    temp_raw[i] <- as.raw("00")
  }
  code <- c(temp_raw,code)
  oracle <- consistent_encryption_oracle(code)
  l <- length(oracle)
  newcode <- oracle[(1+chop*keyl):l]
  return(newcode)
}

base_string <- raw()
base_string <- charToRaw("AAAAAAAAAAAAAAA")
dictionary <- list()
decrypt <- raw()
discover <- list()
num_blocks <- length(consistent_encryption_oracle())/16

for(g in 1:num_blocks){
  discover[[g]] <- raw()
  if (g > 1){
    base_string <- discover[[(g - 1)]]
  }
  for(h in 1:16){
    if (h < 16 && h > 1){
      temp0[1:15] <- c(base_string[(h+1):16],discover[[g]])
      match <- fixed_oracle(base_string[(h+1):16])
    }else if (h == 1){
      temp0[1:15] <- base_string[2:16]
      match <- fixed_oracle(base_string[2:16])
    }else if (h == 16){
      temp0 <- discover[[g]]
      match <- fixed_oracle()
    }
    for(i in 1:255){
      temp <- c(temp0,as.raw(i))
      temp2 <- fixed_oracle(temp)
      if(isTRUE(all.equal(temp2[1:16],match[(1+16*(g-1)):(16+16*(g-1))]))){
        discover[[g]] <- c(discover[[g]],as.raw(i))
      }
    }
    print(h)
  }
}

for(i in 1:num_blocks){
  print(rawToChar(discover[[i]]))
}


#|AAAAAAAX|XXXXXXXY|YYYYYYYZ|