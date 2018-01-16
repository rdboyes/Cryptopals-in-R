input <- "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

start_time <- Sys.time()
bin <- hex_bin(input)
bin_b64(bin)
end_time <- Sys.time()

end_time - start_time #0.0035 s

