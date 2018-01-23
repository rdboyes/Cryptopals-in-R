parseProfile <- function(text){
  poseq = gregexpr('=', text)
  posamp = gregexpr('&', text) 
  names <- ""
  values <- ""
  names[1] <- substring(text,1,(poseq[[1]][1]-1))
  names[2] <- substring(text,posamp[[1]][1]+1,(poseq[[1]][2]-1))
  names[3] <- substring(text,posamp[[1]][2]+1,(poseq[[1]][3]-1))
  values[1] <- substring(text,poseq[[1]][1]+1,(posamp[[1]][1]-1))
  values[2] <- substring(text,poseq[[1]][2]+1,(posamp[[1]][2]-1))
  values[3] <- substring(text,poseq[[1]][3]+1,str_length(text))
  return(cbind(names,values))
}

profileText <- function(object){
  string <- paste0(object[1,1],"=",object[1,2],"&",object[2,1],"=",object[2,2],"&",object[3,1],"=",object[3,2])
  return(string)
}

object <- parseProfile("foo=bar&baz=qux&zap=zazzle")
profileText(object)

profile_for <- function(email){
  error <- str_length(email)
  first_eq <- regexpr("=", email)
  first_amp <- regexpr("&", email)
  if (first_eq < error && first_eq != -1){
    error <- first_eq
  }
  if (first_amp < error && first_amp != -1){
    error <- first_amp
  }
  names <- ""
  values <- ""
  names[1] <- "email"
  values[1] <- substring(email,1,(error))
  names[2] <- "uid"
  values[2] <- "10"
  names[3] <- "role"
  values[3] <- "user"
  return(cbind(names, values))
}

profile <- profile_for("foo@bar.com")
static_key <- random_key()

ecbEncryptProfile <- function(profile){
  profile_enc <- profileText(profile)
  length <- length(charToRaw(profile_enc))
  good_length <- 16*(length %/% 16 + 1)
  padded_code <- pksc(profile_enc, good_length)
  r <- ecbEncrypt(padded_code, key = static_key)
  return(r)
}

ecbDecryptProfile <- function(raw){
  a <- ecbDecrypt(raw, static_key)
  b <- length(raw)
  c <- as.integer(a[b])
  d <- a[1:(b - c)]
  return(parseProfile(rawToChar(d)))
}

ecbEncryptProfile(profile_for("aaaaaaaaaaadmin\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b@gmail.com"))
ecbEncryptProfile(profile_for("RAN@GMAIL.com")) #email=jimmmmmmmmmmmmmm@bob.com&uid=10&role=user
                                                                                           #email=jimmmmmmmmmmmmmm@bob.com&uid=10&role=user
                                          #18
hash <- "6bd8ea7ec450c33e53c3c044a5d882e705c49619bf3623e928af86d85098a963cb3331d6fb95c5ef403400938ffdde0e"
(ecbDecryptProfile(hexString_raw(hash)))

