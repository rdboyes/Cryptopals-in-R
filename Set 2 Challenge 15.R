#PKSC Padding Validation Function

valid_padding <- function(string){
  if(!is.raw(string)){
    raw <- charToRaw(string)
  }
  l <- length(raw)
  num_bytes <- as.integer(raw[l])
  for(i in 1:(num_bytes-1)){
    if(!isTRUE(all.equal(raw[l],raw[(l-i)]))){
      return(FALSE)
    }
  }
  return(TRUE)
}