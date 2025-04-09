############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Substitucni sifra
# substitucni sifra:
substitute_encrypt <- function(text, keyspace) {
  # Připravte mapu z původních znaků na zaměněné znaky
  key <- alphabet$letters[keyspace + 1]
  names(key) <- alphabet$letters
  
  # Převeďte text na číselný vektor
  text_vec <- char_to_numvec(text)
  # Využijte přímo indexování: každý prvek text_vec +1 se stane indexem do "key"
  vec_to_char(key[text_vec + 1])
}

substitute_decrypt <- function(text, keyspace) {
  # Vytvoření šifrovací mapy, stejně jako u šifrování
  key <- alphabet$letters[keyspace + 1]
  names(key) <- alphabet$letters
  
  # Vytvoření inverzní mapy: hodnoty se stávají jmény a naopak.
  inv_key <- names(key)[match(alphabet$letters, key)]
  
  # Převeďte text na číselný vektor a přímo indexujte
  text_vec <- char_to_numvec(text)
  vec_to_char(inv_key[text_vec + 1])
}


set.seed(12345)
key = sample(0:26,27)
key
enc = substitute_encrypt(jlondon,key)

substitute_decrypt(substitute_encrypt(jlondon, key), key)

library(microbenchmark)
benchmark_results <- microbenchmark(
  encrypted = substitute_encrypt(jlondon,key),
  decrypted = substitute_decrypt(enc, key),
  times = 1000
)
benchmark_results


##Pokus o standardni frekvenční prolomení
CZ = CZfreq
CZ = CZ[c(2:27, 1),]
CZ$no = 0:26
CZ=CZ[order(CZ$f),]
CZ

SIF = cetnosti(enc)
SIF = merge(alphabet, SIF, all.x = TRUE)
SIF = SIF[order(SIF$numeric),]
SIF
SIF[is.na(SIF)] = 0
SIF = SIF[order(SIF$f),]
SIF

CZ$SIF = SIF$numeric

CZ=CZ[order(CZ$no),]
CZ

substitute_decrypt(enc, CZ$SIF)

# ukazka toho jak to nefunguje.
km = table(key, CZ$SIF)
sum(diag(km))
sum(diag(km)) / sum(km)

dev.off()
plot(CZ$SIF, type = "p", col = "blue", cex = 2, lwd = 2)
lines(key, type = "p", col = "red")

# proč?
JL <- cetnosti(jlondon)
JL = merge(alphabet, JL, all.x = TRUE)
JL[is.na(JL)] = 0
JL <- JL[order(JL$numeric),]

M = rbind(JL$f,
CZ[order(CZ$no),]$f)
colnames(M) <- CZ[order(CZ$no),]$letters
rownames(M) <- c("SIF","CZ abeceda")
barplot(M, beside = TRUE)
M <- M[, order(M[1,])]
barplot(M, beside = TRUE)


### monte-carlo:
dev.off()
kruh = function(x) (x[1]^2 + x[2]^2) <= 1
plot(c(-1,1), c(-1,1),type = "n")

prostor = seq(-1, 1, length.out=100000000)

r.bod = sample(prostor, 2, TRUE)
r.bod
kruh(r.bod)

# cele dohromady bdueme opakovat!
body = c()
set.seed(12345)
for(i in 1:30000){
  r.bod = sample(prostor,2,TRUE)
  if(kruh(r.bod)){
    col.bod = "blue"
  }else{
    col.bod= "red"
  }
  points(x = r.bod[1],y = r.bod[2],col = col.bod)
  body = c(body, kruh(r.bod))
}

4 * sum(body)/length(body)

konvergence_pi = function(x){
  pix = rep(0,length(x))
  for(i in 1:length(x)) pix[i] = 4 * sum(x[1:i])/i
  pix
}
k <- konvergence_pi(body)
plot(k, type = "l", ylim = c(3, 3.3))
abline(h = pi, col = "red")
tail(k)
head(k,10)

## prechodova matice vytvorena z bigramu
transition_matrix <- function(bigram = CZbigram){
  TM <- matrix(0, 27, 27, dimnames = list(alphabet$letters, alphabet$letters))
  # Rozdělení řetězců bigramů na jednotlivé znaky a jejich převod na čísla
  bigram_chars <- unlist(strsplit(as.character(bigram$letters), ""))
  bigramklic <- matrix(char_to_num(bigram_chars), ncol = 2, byrow = TRUE)
  # Vektorové přiřazení: řádky a sloupce jsou posunuty o 1
  TM[cbind(bigramklic[,1] + 1, bigramklic[,2] + 1)] <- bigram$n
  TM
}


TM = transition_matrix()

TM[TM==0]=1 # neni mozne vypocet provadet pro 0 protoze log(0)

library(lattice)
levelplot(TM)

# generovani pseudonahodneho textu:

first = char_to_num("A")
text = list()
text[[1]] = first
for(i in 1:300){
  second = sample(0:26,1, prob = TM[first+1,]/sum(TM[first+1,]))
  text[[i]] = second
  first = second
}
text
numvec_to_char(unlist(text))

# 
plausibility <- function(text, TM){
  sum(log(TM)*transition_matrix(bigram(text)))
}
plausibility(enc, TM)
plausibility(jlondon, TM)


# Prolomeni substitucni sifry MH algoritmus obecně:
prolom_substitute<-function(text, TM, iter, start = sample(0:26)){
  guess_c = start
  gtext_c = substitute_decrypt(text,guess_c)
  p_c = plausibility(gtext_c,TM)
  
  for(i in 1:iter){
    ij = sample(1:27, 2)
    guess_n = guess_c
    guess_n[ij] = guess_c[ij[2:1]]
    
    gtext_n = substitute_decrypt(text, guess_n)
    p_n = plausibility(gtext_n, TM)
    q = p_n/p_c
    
    if(q>1){
      guess_c = guess_n
      gtext_c = gtext_n
      p_c = p_n
    }else if (q > 0.9 & sample(c(TRUE,FALSE),1,prob=c(0.001,0.999)) ){
      guess_c = guess_n
      gtext_c = gtext_n
      p_c = p_n
    }
    if(i %% 50 ==0){
      print(paste0("|", str_pad(as.character(i), 6, "left"),". iterace, log plaus: ", str_pad(as.character(round(p_c,0)), 12, "left"),"|")) 
    }
    
  }
  print(paste0(rep("-", 60), collapse = ""))
  list(guess_c = guess_c, gtext_c = gtext_c, p_c = p_c)
}

set.seed(3)#3
res = prolom_substitute(enc, TM, 25000)
res
tv = table(res$guess_c,key) #23 z 24, chybí rphodit jen čtyři písmena mezi sebou! :)
sum(diag(tv))


res$gtext_c
jlondon

# % písmen správně umístěných v textu:
sum(strsplit(res$gtext_c, "")[[1]]==strsplit(jlondon, "")[[1]])/nchar(jlondon)


############
############