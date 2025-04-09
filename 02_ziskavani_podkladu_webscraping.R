############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ Webscraping
## stazeni dat krakatit:

web = "https://cs.wikisource.org/wiki/Krakatit"
mainsite = read_html(web)
chapters = mainsite %>% html_elements("#bodyContent ul") %>% .[1] %>% html_nodes("li a") %>% html_attr("href")
  

text = c()
for(chap in chapters) {
  text = c(text, read_html(paste0("https://cs.wikisource.org", chap)) %>% html_nodes(".forma p") %>% html_text)
}

text = str_remove_all(text, "[[:punct:]]|\n")



hackycarky = c("á" = "a", "Á" = "A", "č" = "c", "Č" = "C", "ď" = "d", "Ď" = "D", "é" = "e", "É" = "E,", "ě" = "e","Ě"="E", 
               "í" = "i", "Í" = "I", "ň" = "n", "Ň" = "N", "ó" = "o", "Ó" = "O", "ř" = "r", "Ř"="R", "š" = "s","Š"="S", 
               "ť" = "t", "Ť" = "T", "ú" = "u", "Ú" = "U", "0" = "",  "1" = "", "2" = "", "3" = "","4" = "","5" = "","6" = "","7" = "","8" = "","9" = "", 
               "ů" = "u", "ý" = "y", "Ý" = "y", "ž" = "z", "Ž" = "Z", "ö" = "o", " " = "_", "[+]" = "", "´" = "", "î" = "", "ę" = "", "," = "")

malavelka = c("A","Á","B","C","Č","D","Ď","E","É","Ě","F","G","H","Ch","I","Í","J","K","L","M","N","Ň","O","Ó","P","Q","R","Ř","S","Š","T","Ť","U","Ú","Ů","V","W","X","Y","Ý","Z","Ž")
names(malavelka) = c("a","á","b","c","č","d","ď","e","é","ě","f","g","h","ch","i","í","j","k","l","m","n","ň","o","ó","p","q","r","ř","s","š","t","t","u","ú","ů","v","w","x","y","ý","z","ž")


text = str_replace_all(text, hackycarky)
text = str_replace_all(text, malavelka)
text = paste(text, collapse="_")
text = str_remove_all(text, "[[:blank:]]")
text = str_replace_all(text, "_{2,}","_")
nchar(text)  


substr(text, 1, 100)
sort(unique(char_to_vec(text)))
str(text)


############
############