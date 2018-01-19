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
