############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Vigenerova sifra
## Popis:
### Vigenerova sifra
vigener_encrypt <- function(text, key, alphabet, nrowalphabet){
  text <- char_to_vec(text)
  v.key = char_to_vec(key)
  for(i in 1:(nchar(key))){
    s = seq(from = i, to = length(text), by = nchar(key))
    text[s] = char_to_vec(caesar_encrypt(vec_to_char(text[s]), key = v.key[i], alphabet = alphabet, nrowalphabet = nrowalphabet))
  }
  return(vec_to_char(text))
}

vigener_decrypt <- function(text, key, alphabet,nrowalphabet){
  text <- char_to_vec(text)
  v.key = char_to_vec(key)
  for(i in 1:(nchar(key))){
    s = seq(from = i, to = length(text), by = nchar(key))
    text[s] = char_to_vec(caesar_decrypt(vec_to_char(text[s]), key = v.key[i], alphabet = alphabet, nrowalphabet = nrowalphabet))
  }
  return(vec_to_char(text))
}


vigener_encrypt("AAAAAAAAAAAAAAAAAA","ABC", alphabet =alphabet, nrowalphabet = nrow(alphabet))
sifra = vigener_encrypt(jlondon,"ABC", alphabet = alphabet, nrowalphabet = nrow(alphabet))

vigener_decrypt(vigener_encrypt("AAAAAAAAAAAAAAAAAA","ABC",alphabet =alphabet, nrowalphabet = nrow(alphabet)),"ABC",alphabet =alphabet, nrowalphabet = nrow(alphabet))
vigener_decrypt(sifra,"ABC",alphabet =alphabet, nrowalphabet = nrow(alphabet))

## 
library(microbenchmark)
benchmark_results <- microbenchmark(
  encrypted = vigener_encrypt(jlondon,"ABC",alphabet =alphabet, nrowalphabet = nrow(alphabet)),
  decrypted = vigener_decrypt(sifra,"ABC",alphabet =alphabet, nrowalphabet = nrow(alphabet)),
  times = 1000
)
benchmark_results

## hrubou silou:
enc = vigener_encrypt(jlondon,"JACKLONDON",alphabet =alphabet, nrowalphabet = nrow(alphabet))

prolom_vigener <- function(enc, M_seq = 2:15, alphabet, nrowalphabet){
  keys <- data.frame(matrix(0, nrow = 0, ncol = 3, dimnames = list(c(), c("shift", "klic", "chi"))))
  text = char_to_numvec(enc)
  n = length(text)
  for(shift in M_seq){
    sloupce <- list()
    for(i in 1:shift){
      sloupce[[i]] <- text[seq(from = i, to = n, by = shift)]
    }
    keystream = lapply(sloupce, function(x) prolom_caesar(numvec_to_char(x),alphabet =alphabet, nrowalphabet = nrowalphabet))
    # aplly keystream and decode vigener and count cetnosti, 
    key = paste0(unlist(keystream), collapse = "")
    g.text = vigener_decrypt(enc, key,alphabet =alphabet, nrowalphabet = nrowalphabet)
    
    cet = cetnosti(g.text)
    cet = merge(CZfreq,cet, by = "letters", suffixes = c(".teor",".emp"), all=TRUE)
    cet[is.na(cet)] = 0
    
    chi = sum(((cet$n.emp-cet$f.teor*sum(cet$n.emp))**2)/cet$f.teor*sum(cet$n.emp)) 
    keys = rbind(keys,data.frame(shift = shift, klic = key, chi = chi))
    
  }
  
  keys[order(keys$chi),]
}
prolom_vigener(enc,2:12,alphabet =alphabet, nrowalphabet = nrow(alphabet))
prolom_vigener(enc,20,alphabet =alphabet, nrowalphabet = nrow(alphabet))
vigener_decrypt(enc, "JACKLONDON",alphabet =alphabet, nrowalphabet = nrow(alphabet))


########### kasiski test / metoda:

table_ngram <- function(x, n){
  x = table(ngram_word(x, n))
  x[x > 1]
}
divisors <- function(x){
  y <- seq_len(x)
  y[ x%%y == 0 ]
}

tn = table(ngram_word(enc, 9))

tn[tn>1]

kasiski <- function(repword,text){
  r = gregexpr(repword,text)[[1]][1:2]
  rozdil = r[2] - r[1]
  
  divisors(rozdil)
}

kasiski("IJKXZINWY",enc)
kasiski("CMVDOVKKP",enc)


# Friedman:
sum((CZfreq$n*(CZfreq$n-1))/(sum(CZfreq$n)*(sum(CZfreq$n-1))))
ICcz = sum(CZfreq$f**2)
ICcz

# odhad delky (+-):
friedman <- function(text, IC = ICcz, n = 27){
  m = (IC-1/n) / (sum(cetnosti(text)$f**2)-1/n)
  m
}

friedman(text = jlondon)
friedman(text = enc)
prolom_vigener(enc, c(5, 10),alphabet =alphabet, nrowalphabet = nrow(alphabet))
# -> dekla klice by byla 5 a nebo 10, pak uz brutal force
# tento postu porezání kombinací se pouzívá i u moderních šifer

vigener_decrypt(enc, "JACKLONDON",alphabet =alphabet, nrowalphabet = nrow(alphabet))

############
############