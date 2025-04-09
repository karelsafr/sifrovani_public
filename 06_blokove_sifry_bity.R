############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Blokove sifry

alphabet = data.frame(letters = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", 
                                  "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_"),
                      numeric = c(0:26))

jlondon = c(read.table("./data/jlondon.txt")[,1])
jlondon

# Krakatit, Capek, 434385 znaků
krakatit = c(read.table("./data/Krakatit.txt")[,1])
str(krakatit)


# funkce na prevody
textTobit64Matrix <- function(x) {
  x  = rawToBits(as.raw(utf8ToInt(x)))
  matrix(x, ncol = 64, byrow = TRUE)
}

intTobit64Matrix <- function(x) {
  x  = rawToBits(as.raw(x))
  matrix(x,ncol = 64, byrow = TRUE)
}

x=textTobit64Matrix("NASETAJE")
x

bit64MatrixToText <- function(x){
  x=c(t(x))
  intToUtf8(as.integer(packBits(x)))
}

bit64MatrixToInt <- function(x){
  x = c(t(x))
  (as.integer(packBits(x)))
}

bit64MatrixToText(textTobit64Matrix("testtesttesttest"))
intTobit64Matrix(c(1:16))

## sifrovani:

bin_enc <- function(bin_x, bin_key){
  xor(bin_x, bin_key)
}

bin_dec <- function(bin_x, bin_key){
  xor(bin_x, bin_key)
}


## Lookup tabulky pro S funkce:
#lookuptabulky:
LT = matrix(data = c(
  7,14,1,11,15,13,3,8,6,9,12,10,5,2,4,0,
  10,1,6,13,9,14,5,12,11,3,7,2,4,0,15,8,
  0,3,7,4,12,14,10,2,11,15,8,9,6,13,5,1,
  5,9,4,7,3,11,13,10,6,14,15,8,12,2,1,0,
  14,6,1,5,12,2,8,15,7,4,9,10,0,13,3,11,
  10,9,5,1,15,0,2,14,12,4,6,7,11,8,13,3,
  15,7,14,1,12,5,3,11,8,10,13,0,9,4,6,2,
  9,13,8,6,11,5,14,15,3,0,4,2,7,10,1,12,
  1,13,9,12,15,10,8,5,4,6,0,7,2,14,3,11,
  8,13,12,5,10,4,2,3,9,7,1,15,0,6,11,14,
  15,3,6,9,5,12,0,7,11,4,2,13,10,14,8,1,
  8,0,13,1,5,14,10,2,3,9,7,6,4,12,15,11,
  5,7,4,13,10,1,2,8,15,0,14,3,9,6,11,12,
  14,11,13,0,10,15,2,4,7,6,1,3,12,9,5,8,
  9,3,4,11,6,8,1,7,10,0,12,13,2,5,14,15,
  6,14,1,7,15,13,10,9,8,3,0,4,2,11,5,12
), ncol = 16, byrow = TRUE)
colnames(LT) = 0:15

keyLT <-matrix(0, ncol = 16, nrow = 2)
keyLT[1, ] <-c(8,10,9,0,6,7,1,15,2,3,13,4,12,11,5,14)
keyLT[2, ] <-c(0,1,8,14,11,9,6,10,2,3,7,13,5,12,15,4)

iLT<-LT
for(i in 1:16) iLT[i,] <- c(0:15)[order(LT[i,])]

#
matrix(rawToBits(as.raw(0:15)), byrow = TRUE, ncol = 8)[,1:4]

# 4 bity jsou čisla od 0 do 15ti, 
# dve funkce na konverze 4 bit formatu:
bit4ToInt <- function(x){
  as.integer(packBits(c(x, rep(as.raw(0), 4))))
}

intToBit4 <- function(x){
  rawToBits(as.raw(x))[1:4]
}
zpracovani = sort(rep(1:16,4))

bit64ToInt <- function(x, zpracovani_vec = zpracovani){
  as.numeric(tapply(x, zpracovani_vec, bit4ToInt))
}

intTo64bit <- function(x){
  c(sapply(x, intToBit4))
}


# S funkce:
S_enc <- function(x, enc=LT){
  x = bit64ToInt(x)
  for(i in 1:16) x[i] = enc[i, x[i]+1]
  intTo64bit(x)
}

S_dec <- function(x, dec=iLT) {
  x = bit64ToInt(x)
  for(i in 1:16) x[i] = dec[i, x[i]+1]
  intTo64bit(x)
}


S_key <- function(x, keyenc){
  x = bit4ToInt(x)
  x = keyenc[x+1]
  intTo64bit(x)
}

slovo = "slovojan"
heslo = "hesloten"

# zasifrovani / odsifrovani bez michani:
r=S_enc(bin_enc(textTobit64Matrix(slovo),textTobit64Matrix(heslo)))
bit64MatrixToText(bin_dec(S_dec(r), textTobit64Matrix(heslo)))

## michani:
bit_mix = c(0,16,32,48,1,17,33,49,2,18,34,50,3,19,35,51,
  4,20,36,52,5,21,37,53,6,22,38,54,7,23,39,55,
  8,24,40,56,9,25,41,57,10,26,42,58,11,27,43,59,
  12,28,44,60,13,29,45,61,14,30,46,62,15,31,47,63)+1
# R pozicuje od 1!
bit_demix <- c(1:64)[order(bit_mix)]

bitmix <- function(x, mixing = bit_mix){
  x[mixing]
}
bitmix(1:64)
bitdemix <- function(x, mixing = bit_demix){
  x[mixing]
}
bitdemix(bitmix(1:64))

# Round na jednom bloku
bitblok_enc <- function(x, key){
  bitmix(S_enc(bin_enc(x,key)))
}
bitblok_dec <- function(x, key){
  bin_dec(S_dec(bitdemix(x)),key)
}
slovo
r=bitblok_enc(textTobit64Matrix(slovo),textTobit64Matrix(heslo))
r
bit64MatrixToText(bitblok_dec(r,textTobit64Matrix(heslo)))

# hurá!

# zacisteni delky textu na bloky po 8 pismenech (64bit)
textTo8 <- function(x){
  zbytek = nchar(x) %% 8
  if(zbytek > 0) x = paste0(x,paste0(sample(alphabet$letters,8-zbytek), collapse = ""))
  x
}

textTo8("T")
# zacisteni a replikace hlesla:
hesloTo16 <- function(x){
  substr(paste0(rep(x, 16), collapse = ""), 1, 16)
}
hesloTo16("te")
hesloTo16("texttextjedna")

# keystream | round key 
tk = "heslocojetonaseh"
Kspace <- function(tk){
  tk = hesloTo16(tk)
  tk = c(t(textTobit64Matrix(tk)))
  K = matrix(raw(0), ncol = 128, nrow = 32)
  K[1,]= tk
  for(i in 2:32){
    rk = K[i-1,]
    rk = rk[c(66:128, 1:65)] # left rotation by 61 bit
    rk[1:4] = S_key(rk[1:4], keyLT[1,])# S 
    rk[5:8] = S_key(rk[5:8], keyLT[1,])# S
    rk[62:66] =  xor(rk[62:66], intToBits(i-1)[1:5])
    K[i,] = rk
  }
  K
}

Kspace(tk)
Kspace("test")
# vse dohromady:

present_enc <- function(text, key){
  key = Kspace(key)
  text = textTo8(text)
  text = textTobit64Matrix(text)
  
  for(round_i in 1:32){
    for(i in 1:nrow(text)){
      text[i,] = bitblok_enc(text[i,], key[round_i, 1:64])
    }
  }
  bit64MatrixToInt(text)
}

present_dec <- function(inttext, key){
  key = Kspace(key)
  text = intTobit64Matrix(inttext)
  
  for(round_i in 32:1){
    for(i in 1:nrow(text)){
      text[i,] = bitblok_dec(text[i,], key[round_i, 1:64])
    }
  }
  bit64MatrixToText(text)
}


r = present_enc("test", "test")
r
present_dec(r, "test")


textkzasifrovani = substr(krakatit, 1, 10000)


zasifrovane = present_enc(textkzasifrovani, "NASEHESLO")
zasifrovane2 = present_enc(textkzasifrovani, "NASEHESLA")
par(mfrow = c(2, 1))
barplot(table(zasifrovane), xlab = "UTF-8 int", ylab = "abs cetnost", main = "zasifrovany text")
barplot(table(utf8ToInt(textkzasifrovani)), xlab = "UTF-8 int",ylab = "abs cetnost", main = "puvodni text")

present_dec(zasifrovane, "NASEHESLO")

#citlivost na zmenu hesla:
present_enc("ABCDEFGHIJKLM", "NASEHESLO")
present_enc("ABCDEFGHIJKLM", "NASEHESLA")
#citlivost na zmenu textu:
present_enc("ABCDEFGHIJKLM", "NASEHESLO")
present_enc("BBCDEFGHIJKLM", "NASEHESLO")



############
############