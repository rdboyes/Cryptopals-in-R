input <- "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"
key <- "ICE"

plaintext_bin <- function(text){
  length <- str_length(text)
  pt_array <- array()
  text_array <- array()
  num_array <- array(data = 0)
  for (i in 1:length){
    text_array[i] <- substring(text, i, i)
    num_array[i] <- as.numeric(charToRaw(text_array[i]))
  }
  m <- matrix(0, nrow = length*2, ncol = 4)
  for (k in 1:length){
    m[k*2-1,1] <- num_array[k]%/%128
    m[k*2-1,2] <- (num_array[k] - m[k*2-1,1]*128)%/%64
    m[k*2-1,3] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64)%/%32
    m[k*2-1,4] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32)%/%16
    m[k*2,1] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16)%/%8
    m[k*2,2] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8)%/%4
    m[k*2,3] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4)%/%2
    m[k*2,4] <- (num_array[k] - m[k*2-1,1]*128 - m[k*2-1,2]*64 - m[k*2-1,3]*32 - m[k*2-1,4]*16 - m[k*2,1]*8 - m[k*2,2]*4 - m[k*2,3]*2)
  }
  return(m)
}

goal <- "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
inHex <- bin_hex(plaintext_bin(input))
keyHex <- bin_hex(plaintext_bin(key))
if (bin_hex(hex_xor(inHex,keyHex)) == goal) print("Match")
