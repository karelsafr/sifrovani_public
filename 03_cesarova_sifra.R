############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Cesarova sifra
## Popis:
# Caesar:
caesar_encrypt  <- function(text, key = num_to_char(n), n = char_to_num(key), alphabet, nrowalphabet = nrow(alphabet)){
  numvec_to_char((char_to_numvec(text) + n) %% nrowalphabet)
}

caesar_decrypt  <- function(text, key = num_to_char(n), n = char_to_num(key),alphabet, nrowalphabet = nrow(alphabet)){
  numvec_to_char((char_to_numvec(text) - n) %% nrowalphabet)
}


#test:
encrypted = caesar_encrypt(jlondon, n = 16, alphabet = alphabet)
encrypted
decrypted = caesar_decrypt(encrypted, n = 16, alphabet = alphabet)
decrypted

caesar_decrypt(caesar_encrypt(jlondon, n = 16, alphabet = alphabet), n = 16, alphabet = alphabet)

###### benchmark vůči jednodušší implementaci:

library(microbenchmark)


radek = nrow(alphabet)
benchmark_results <- microbenchmark(
  encrypted = caesar_encrypt(jlondon, n = 16, alphabet = alphabet, nrowalphabet = radek),
  decrypted = caesar_decrypt(jlondon, n = 16, alphabet = alphabet, nrowalphabet = radek),
  times = 1000
)

benchmark_results

## test:
par(mfrow = c(9,3), mar = c(0, 0.25, 1.5, 0.25))
enclondon = caesar_encrypt(jlondon, key = "F",alphabet = alphabet, nrowalphabet = radek)
for(i in 1:27){
  cet = cetnosti(caesar_decrypt(enclondon, n = i-1,alphabet = alphabet, nrowalphabet = radek))
  # merge na CZfreq!
  cet = merge(CZfreq, cet, by = "letters",suffixes = c(".teor", ".emp"), all = TRUE)
  
  plot(cet$f.teor,type = "b", col = "darkgrey", ylim = c(0, 0.2), xaxt = "n",
       yaxt = "n", main = alphabet[i,]$letters)
  lines(cet$f.emp,type = "b", col = "red")
}

chitest = alphabet
chitest$chi = c(0)
for(i in 1:27){
  cet = cetnosti(caesar_decrypt(enclondon, n = i-1,alphabet = alphabet, nrowalphabet = radek))
  cet = merge(CZfreq,cet, by = "letters",suffixes = c(".teor", ".emp"),all = TRUE)
  cet[is.na(cet)] = 0
  
  chitest$chi[i] = sum(((cet$n.emp-cet$f.teor*sum(cet$n.emp))**2)/cet$f.teor*sum(cet$n.emp))
}
chitest

prolom_caesar <- function(text,alphabet =alphabet, nrowalphabet = nrow(alphabet)){
  chitest<-c()
  for(i in 1:nrow(alphabet)){
    cet = cetnosti(caesar_decrypt(text, n = i-1,alphabet = alphabet, nrowalphabet = radek))
    cet = merge(CZfreq,cet, by = "letters",suffixes = c(".teor",".emp"), all = TRUE)
    cet[is.na(cet)] = 0
    chitest[i] = sum(((cet$n.emp-cet$f.teor*sum(cet$n.emp))**2)/cet$f.teor*sum(cet$n.emp))
  }
  alphabet[order(chitest)[1],]$letters
}

# automatizace
text = caesar_encrypt(jlondon,"F",alphabet=alphabet)

prolom_caesar(text,alphabet =alphabet, nrowalphabet = nrow(alphabet))

caesar_decrypt(text,"F",alphabet = alphabet)


microbenchmark(
  encrypted = prolom_caesar(text,alphabet =alphabet, nrowalphabet = nrow(alphabet)),
  times = 100
)


############
############