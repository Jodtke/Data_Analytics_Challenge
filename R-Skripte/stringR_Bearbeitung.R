########### Erstellung Items6 mit strinR Package für Item-Tokenization & Word-Mining #########
library(tidyverse)
### daten einlesen
items <- read_csv(file="./Data/items5.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_character(),
  author=col_character(),
  publisher=col_character(),
  main.topic=col_factor(),
  subtopics=col_factor()
))
head(items, n=20)
glimpse(items)

### utf8 überall encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"))
### whitespace von beiden seiten entfernen
items[, 2:4] <- apply(items[, 2:4], MARGIN=2, FUN=function(x) str_trim(string=x,side="both"))
### alle strings in Großbuchstaben darstellen
items[, 2:4] <- sapply(X=items[,2:4], FUN=function(x) str_to_upper(string=x, locale="de")) # locale="english" im default...besser als Deutsch?
### komische sonderzeichen ersetzen durch "" 
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""))
#items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:alnum:]]", replacement=" "))

### neues csv "items6" schreiben
write_csv(items, file="./Data/items6.csv", col_names=T)
## testweise einlesen
items <- read_csv(file="./Data/items6.csv", col_names=T)
head(items, n=20)
