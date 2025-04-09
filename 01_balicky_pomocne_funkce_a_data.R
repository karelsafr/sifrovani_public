############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Balicky_pomocne_funkce_a_data, 2025
## Balicky:

packages_for_workshop<-c("stringi", "nloptr", "tm", "reader", "plotrix", "stringr",
                         "DescTools", "purrr", "rvest", "stringr", "ggplot2", "docstring")

for(p in packages_for_workshop){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


## pomocna data:
#např: 
setwd("~/Dokumenty/GitHub/SIFROVANI_IT_SAFR/V2_0")

# Jack London 1449 znaků
jlondon = c(read.table("./data/jlondon.txt")[,1])
jlondon

# Krakatit, Capek, 434385 znaků
krakatit = c(read.table("./data/Krakatit.txt")[,1])
str(krakatit)

# Abeceda:
alphabet = data.frame(letters = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", 
                                 "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_"),
                     numeric = c(0:26))


alphabet


## Pomocne funkce
# string do vektoru:
char_to_vec <- function(x) strsplit(x, "")[[1]]
char_to_vec("karel") # test

# vektor charakteru do charakteru:
vec_to_char <- function(x) paste0(x, collapse = "")
vec_to_char(c("t","e","s","t")) #test

vec_to_char(char_to_vec("cesar")) #test navaznosti obou funkci


# prevod textu na cislo dle abecedy
char_to_num <- function(x){
  text = data.frame(n=1:length(x), letters = x)
  M = merge(text, alphabet, by = "letters", all.x = TRUE, all.y = FALSE, sort = FALSE)
  M = M[order(M$n),]
  return(M$numeric)
}
char_to_num(c("T","E","S","T"))

# prevod cisla na text (zpet)
num_to_char <- function(x){
  cisla = data.frame(n = 1:length(x), numeric = x)
  M = merge(cisla, alphabet, by = "numeric", all.x = TRUE, all.y = FALSE, sort = FALSE)
  M = M[order(M$n),]
  return(M$letters)
}
num_to_char(c(19, 4, 18, 19))

# test pouziti dohromady:
char_to_vec(jlondon)
char_to_num(char_to_vec(jlondon))

num_to_char(char_to_num(char_to_vec(jlondon)))
vec_to_char(num_to_char(char_to_num(char_to_vec(jlondon))))

# vectors:
char_to_numvec <- function(x) char_to_num(char_to_vec(x))
numvec_to_char <- function(x) vec_to_char(num_to_char(x))

char_to_numvec(jlondon)
numvec_to_char(char_to_numvec(jlondon))


## ngramy:
# n-gram
ngram_word <- function(word, n){
  if(nchar(word) >= n){
    ngrams <- c()
    word <- char_to_vec(word)
    while(length(word) >= n) {
      ngrams = append(ngrams, vec_to_char(word[1:n]))
      word = word[-c(1)]
    }
    return(ngrams)
  }else{
    return(NULL)
  }
  
  
}
ngram_word("test",2)
ngram_word("test",3)

ngram <- function(text, n = 2, sep = "_"){
  words = str_split(text, sep)[[1]]
  words = words[nchar(words) >= n]
  
  ngrams=sapply(words, ngram_word, n)
  return(table(unlist(ngrams)))
  
}

bigram <- function(text){
  vtext = char_to_vec(text)
  if(length(vtext) %% 2 == 0){
    MB=matrix(vtext, ncol = 2, byrow = TRUE)
  }else{
    MB=matrix(c(vtext[-length(vtext)]), ncol = 2, byrow = TRUE)
  }
  fb = paste0(MB[,1], MB[,2])
  vtext = vtext[-1]
  if(length(vtext) %% 2 == 0){
    MB = matrix(vtext, ncol = 2, byrow = TRUE)
  }else{
    MB = matrix(c(vtext[-length(vtext)]), ncol = 2, byrow = TRUE)
  }
  sb = paste0(MB[,1], MB[,2])
  bigrams = c(fb, sb)
  b = table(bigrams)
  bigrams = data.frame(bigrams = names(b), Freq = c(b))
  bigrams$f = bigrams$Freq/sum(bigrams$Freq)
  colnames(bigrams)[1:2] <- c("letters","n")
  bigrams
}



# obecna funkce na vytezeni freqkvenci:
cetnosti  <- function(text, ng = 1,sep = ""){
  tf = ngram(text, ng,sep)
  tf = data.frame(tf)
  tf$f = tf$Freq/sum(tf$Freq)
  colnames(tf)[1:2] = c("letters", "n")
  return(tf)
}

# CZbigram=bigram(krakatit)
# CZfreq = cetnosti(krakatit, ng = 1, sep="")

CZfreq = read.csv2("./data/CZfreq.csv")
head(CZfreq)

CZbigram = read.csv2("./data/CZbigram.csv", na.strings = "NNA") #problem s NA string!!!
head(CZbigram)


############ Optimalizované funkce (uspora cca 10x rychlejší)
############  


# jednorázově definovaná mapa:
alphabet_map <- setNames(seq_len(nrow(alphabet)) - 1, alphabet$letters)

char_to_num <- function(char) {
  # předpokládejme, že char je jeden znak
  alphabet_map[char]
}

char_to_numvec <- function(text) {
  # rozdělte text na jednotlivé znaky a využijte rychlé indexování
  unname(alphabet_map[strsplit(text, "")[[1]]])
}

numvec_to_char <- function(numvec) {
  # invertovaná mapa – opět vytvořená jednorázově
  inv_map <- names(alphabet_map)[match(numvec, alphabet_map)]
  paste(inv_map, collapse = "")
}

# table pocita rychleji nez merge
cetnosti <- function(text) {
  chars <- strsplit(text, "")[[1]]
  counts <- table(factor(chars, levels = alphabet$letters))
  data.frame(letters = names(counts), n = as.numeric(counts), f = as.numeric(counts)/sum(as.numeric(counts)))
}
cetnosti(jlondon)

#### optimalizovaná verze
bigram <- function(text) {
  vtext <- char_to_vec(text)         # Převede text na vektor znaků
  n <- length(vtext)
  if (n < 2) return(data.frame(letters = character(0), n = numeric(0), f = numeric(0)))
  
  # Vytvoří bigramy jako posuvné dvojice: (vtext[i], vtext[i+1]) pro i = 1 až n-1
  bigrams <- paste0(vtext[-n], vtext[-1])
  
  # Vypočítá četnosti a relativní frekvence
  b <- table(bigrams)
  df <- data.frame(letters = names(b), n = as.vector(b), stringsAsFactors = FALSE)
  df$f <- df$n / sum(df$n)
  
  df
}

