#Challenge 16: CBC Bitflipping attacks

create_user_data <- function(userdata){
  output <- paste0("comment1=cooking%20MCs;userdata=",userdata,";comment2=%20like%20a%20pound%20of%20bacon")
  return(output)
}
