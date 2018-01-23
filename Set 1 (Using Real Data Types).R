library(stringr)

#Task 1: Covert hex to Base64

input <- "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexString_raw <- function(input){
  l <- 0.5 * str_length(input)
  in_raw <- raw(l)
  for(i in 1:(length(in_raw))){
    in_raw[i] <- as.raw(as.hexmode((substring(input,(i*2-1),(i*2)))))
  }
  return(in_raw)
}
 
out <- base64enc::base64encode(hexString_raw(input))
out

#Task 2: XOR two hex strings

#Write a function that takes two equal-length buffers and produces their XOR combination.

#If your function works properly, then when you feed it the string:
input1 <- "1c0111001f010100061a024b53535009181c"
#... after hex decoding, and when XOR'd against:
input2 <- "686974207468652062756c6c277320657965"
#... should produce:
goal_output <- "746865206b696420646f6e277420706c6179"

result <- xor(hexString_raw(input1),hexString_raw(input2))

assertthat::are_equal(result, hexString_raw(goal_output))

#Task 3: Single Char XOR
input <- "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
in_raw <- hexString_raw(input)

raw_single_char <- function(in_raw){
  library(nnet)
  solution <- list()
  score <- 0
  for(i in 1:255){
    key <- as.raw(i)
    solution[[i]] <- xor(in_raw,key)
    score[i] <- score_raw(solution[[i]])
  }
  return(solution[[which.is.max(score)]])
}

score_raw <- function(raw_input){
  score <- 0
  common_letters <- c("65","74","61","69","6f","6e","20","72","68","72","64","6c","75")
  common_letters_raw <- raw()
  not_letters_raw <- raw()
  for(i in 1:length(common_letters)){
    common_letters_raw[i] <- hexString_raw(common_letters[i]) 
  }
  not_letters <- c(seq(1,19),seq(21,30))
  not_letters_raw <- as.raw(not_letters)
  for(i in 1:length(raw_input)){
    if (raw_input[i] %in% common_letters_raw) {
      score <- score + 10
    }else if (raw_input[i] %in% not_letters_raw){
      score <- score - 5
    }
  }
  return(score)
}

rawToChar(raw_single_char(in_raw))

#4. Detect Single Char XOR

library(readr)
xor_text <- read_csv("Downloads/4.txt", col_names = FALSE)

solutions_4 <- list()
score <- 0
for(i in 1:327){
  solutions_4[[i]] <- raw_single_char(hexString_raw(xor_text[i,1]))
  score[i] <- score_raw(solutions_4[[i]])
}
rawToChar(solutions_4[[which.is.max(score)]])

#5. Repeating Key XOR

input <- "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"
key <- "ICE"

goal <- "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
in_raw <- charToRaw(input)
key_raw <- charToRaw(key)

raw_xor <- function(long, short){
  l1 <- length(long)
  l2 <- length(short)
  temp <- short
  repeats <- (l1 %/% l2) - 1
  extras <- l1 %% l2
  for(i in 1:repeats){
    temp <- c(temp, short)
  }
  if (extras != 0){
    for(i in 1:extras){
      temp <- c(temp, short[i])
    }
  }
  return(xor(temp,long))
}

assertthat::are_equal(raw_xor(in_raw, key_raw), hexString_raw(goal))

#6. Break Repeating Key XOR

#see separate file

#7. AES in ECB

b64_text <- read_csv("/Users/Randy/Downloads/7.txt", col_names = FALSE)

ecbDecrypt <- function(text, key){
  library(openssl)
  library(digest)
  aes <- AES(mode = "ECB",key = charToRaw(key))
  msg <- base64_decode(as.matrix(b64_text))
  result <- aes$decrypt(msg)
  return(result)
}

ecbDecrypt(b64_text, "YELLOW SUBMARINE")

#8. Detect ECB

hex_text <- read_csv("/Users/Randy/Downloads/8.txt", col_names = FALSE)
raw_list <- list()
for(i in 1:204){
  raw_list[[i]] <- hexString_raw(hex_text[i, 1])
}

detect <- FALSE
for(i in 1:204){
  detect[i] <- is_ecb(raw_list[[i]])
}

is_ecb <- function(raw_hex){
  matchCount <- 0
  stringLength <- (length(raw_hex) - 8)
  for(j in 1:(stringLength)){
    ref <- raw_hex[(j):(j+7)]
    for(k in j:stringLength){
      test <- raw_hex[(k):(k+7)]
      if(isTRUE(all.equal(ref,test))){
        matchCount <- matchCount + 1  
      }
    }
  }
  if(matchCount > stringLength){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

