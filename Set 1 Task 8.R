hex_text <- read_csv("/Users/Randy/Downloads/8.txt", col_names = FALSE)

stringLength <- (str_length(hex_text[1,1]) - 16)/16
for(i in 1:204){
  matchCount[i] <- 0
  for(j in 1:(stringLength)){
    ref <- substring(hex_text[i,1],(j*16),(j*16+15))
    for(k in 1:stringLength){
      test <- substring(hex_text[i,1],(k*16),(k*16+15))
      if (ref == test){
        matchCount[i] <- matchCount[i] + 1
      }
    }
  }
}

print("The ECB encrypted hex is at position")
print(which.max(matchCount))