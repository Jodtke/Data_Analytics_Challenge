########### Erstellung Items6 mit strinR Package für Item-Tokenization & Word-Mining #########
library(tidyverse)
### daten einlesen
items <- read_csv(file="./Data/items6.csv", col_names=T, col_types=cols(
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
items[, 2:4] <- sapply(X=items[,2:4], FUN=function(x) str_to_lower(string=x, locale="de")) # locale="english" im default...besser als Deutsch?
### komische sonderzeichen ersetzen durch "" 
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""))
#items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:alnum:]]", replacement=" "))
head(items, n=20)

### subTopics von sonderzeichen befreien
items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)
items$subtopics <- gsub(","," ",items$subtopics)
head(items, n=20)

### mainTopics und subTopics zusammenführen und subtopics als einzelne variable entfernen
items <- items %>%
  mutate(uniteTopics = paste(main.topic, subtopics, sep =  " ")) %>%
  select(-subtopics)
head(items, n=20)
### dopplungen aus maintopics und subtopics in variable 'uniteTopics' entfernen und mainTopics "kleiner" machen
rem_dup_word <- function(x) {
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x, split=" ", fixed=F, perl=T) ), which="both" ) ), collapse= " " )
}

### neue variable items_bearb definieren (damit bei fehler nicht immer skript neu gestartet werden muss!)
items_bearb <- items

items_bearb <- items_bearb %>%
  mutate(
    newTopics = unlist(sapply(uniteTopics, FUN=rem_dup_word, USE.NAMES=F))) %>%
  select(-uniteTopics) %>%
  rename(uniteTopics = newTopics, mainTopic = main.topic) %>%
  mutate(
    mainTopic = tolower(mainTopic))
head(items_bearb, n=20)
str(items_bearb)

items <- items_bearb
head(items, n=20)
rm(items_bearb, rem_dup_word)

### neues csv "items7" schreiben
#write_csv(items, file="./Data/items7.csv", col_names=T)
### testweise einlesen
# items2 <- read_csv(file="./Data/items7.csv", col_names=T, col_types=cols(
#   itemID=col_factor(),
#   title=col_character(),
#   author=col_character(),
#   publisher=col_character(),
#   mainTopic=col_factor(),
#   uniteTopics=col_factor()
# ))
# head(items2, n=20) # hat Geklappt!

################ crawler daten bearbeiten ############
# Import der Crawler-Daten
FCD <- readRDS("./Data/FinaleCrawlerDatenUpdated.rds")

# https://stackoverflow.com/questions/49564748/extract-multiple-elements-from-a-list-of-lists-lapply
FCD_tibble <- as_tibble(do.call("rbind", lapply(FCD, '[', c(1, 15))))
# Anzahl der NAs
sum(is.na(FCD_tibble$Beschreibung))
# 13732 Items haben keine Klappentexte

### Tibble aus den Titeln und Beschreibungen der Klappentexte erstellen
FCD_tibble <- FCD_tibble %>% 
  rename(title = Titel) %>% 
  mutate(title = unlist(title)) %>% 
  mutate(Beschreibung = unlist(Beschreibung))
rm(FCD)
head(FCD_tibble, n=20)

### crawler daten an itemliste anpassen
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_conv(string=x, encoding="UTF-8"), USE.NAMES=F)
### whitespace von beiden seiten entfernen
FCD_tibble[, 1:2] <- apply(X=FCD_tibble[, 1:2], MARGIN=2, FUN=function(x) str_trim(string=x, side="both"))
### alle strings in Großbuchstaben darstellen
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_to_lower(string=x, locale="de"), USE.NAMES=F) # locale="english" im default...besser als Deutsch?
### komische sonderzeichen ersetzen durch "" 
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""), USE.NAMES=F)
#items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:alnum:]]", replacement=" "))
head(FCD_tibble, n=20)

### als eigene csv-Datei abspeichern
#write_csv(FCD_tibble, file="./Data/KlappentexteUndTitel.csv", col_names=T)
### testweise einlesen
# FCD_tibble2 <- read_csv(file="./Data/KlappentexteUndTitel.csv", col_names=T, col_types=cols(
#    title=col_character(),
#    Beschreibung=col_character()
# ))
# head(FCD_tibble2, n=20) # hat Geklappt!

