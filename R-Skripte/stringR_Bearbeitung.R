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
  mutate(uniteTopics = paste(main.topic, subtopics, sep =  " ")) %>% # ACHTUNG: Komma nur übergangsweise drin, kann wieder gelöscht werden
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

######### dataframe erstellen im longer format für author und topic als key (entscheidungsvariable) mit jeweiligem topic dahinter ########

### items datensatz für bearbeitung neu und extra einlesen, da kommas zwischen den subtopics NICHT entfernt werden
# NICHT ALS NEUE ITEM DATEI ABSPEICHERN!
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
#items$subtopics <- gsub(","," ",items$subtopics)
head(items, n=20)

### mainTopics und subTopics zusammenführen und subtopics als einzelne variable entfernen
items <- items %>%
  mutate(uniteTopics = paste(main.topic, subtopics, sep = ",")) %>% # ACHTUNG: Komma nur übergangsweise drin, kann wieder gelöscht werden
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
#rm(items_bearb, rem_dup_word)

#### funktion um einzelne topics in extra variablen aufzuspalten
split_into_multiple <- function(column, pattern = ",", into_prefix) {
  cols <- str_split_fixed(column, pattern, n=Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as_tibble(cols)
  # jeweilige columns benannt mit "columnName"_Zahl 
  # where m = Anzahl an Columns für Zahlenbenennung
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

items_bearb2 <- items_bearb %>% 
  bind_cols(split_into_multiple(.$uniteTopics, ",", "topic")) %>% 
  # nur Spalten wählen mit Prefix "_"
  select(author, starts_with("topic_"))
#### nur 33 spalten variablen !?

items_bearb3 <- items_bearb2 %>% 
  gather(key, val, -author, na.rm = T) %>% # author als linke wichtige entscheidungsvariable lassen
  rename(numberTopic=key, topic=val) %>% ### key und value echte variablen-namen geben
  group_by(author, topic) %>%
  summarise(n=n()) %>%
  spread(key=topic, value=n)
str(items_bearb3)
head(items_bearb3, n=20)
rm(items, items_bearb, rem_dup_word, split_into_multiple)

##############cosine matrix erstellen zwischen autoren und belegung der jeweiligen topics ##########
# Matrix aufbauen, mit den Dimensionen entsprechend der Anzahl der Autoren und Genre
AuthMat_subtopics <- items_bearb3 #%>% 

# Autorennamen abspeichern, da diese später als rownames eingefügt werden
Authors_subtopics <- AuthMat_subtopics$author

apply(X=AuthMat_subtopics, MARGIN=2, FUN=class)

# komischerweise werden die Spalten als character dargestellt, deswegen müssen die Variablen
# in numerische Werte transformiert werden, damit die Korrelationen zwischen den Autoren berechnet
# werden können

AuthMat_subtopics <- apply(X=AuthMat_subtopics[, 2:ncol(AuthMat_subtopics)], MARGIN=2, FUN=as.integer)

# Überprüfung, ob Transformation funktioniert hat
apply(X=AuthMat_subtopics, MARGIN=2, FUN=class)
head(AuthMat_subtopics, n=20)

# Nun befindet sich die Matrix in der gewünschten Formatierung:
# In den Zeilen stehen die Autoren und in den Spalten die Main Topics
# Dazu kommt, dass die Spalten nun auch alle numerisch sind, folglich können
# darauf nun similarity-Algorithmen angewendet werden.

# Grundsätzlich bieten sich hierfür folgende drei an:
# 1. Pearson Correlation
# 2. Jaccard Coefficient
# 3. Cosine Similarity

# Während für die Pearson Correlation schon eine Implementation in {stats}
# vorhanden ist, benötigen wir für die anderen beiden ein extra package.
# Hierfür wählen wir das Package "proxy" da wir mit diesem sowohl den Jaccard
# Coefficient berechnen können, als auch die Cosine Similarity

# Für kleine bis mittelgroße Matrizen
# install.packages("proxy")
# library(proxy)

# Für große Matritzen
# install.packages("proxyC")
# library(proxyC)

#AuthorCor <- cor(t(AuthMat), use = "pairwise.complete.obs", method = "pearson")

# Leider hat die Funktion simil aus dem proxy-Package Probleme, wenn sich NAs
# im Datensatz befinden. Foglich müssen wir diese alle zunächst in Nullen
# konvertieren, damit as Ergebnis nicht verzerrt wird:

AuthMat0_subtopics <- AuthMat_subtopics
AuthMat0_subtopics[is.na(AuthMat0_subtopics)] <- 0
mode(AuthMat0_subtopics) <- "integer"
# dim(AuthMat0)
# length(Authors)
rownames(AuthMat_subtopics) <- Authors_subtopics
rownames(AuthMat0_subtopics) <- Authors_subtopics

AuthMat0sparse_subtopics <- as(AuthMat0_subtopics, "sparseMatrix")

## Anwendung von proxyC -#######################################################

rm(Authors_subtopics)
rm(AuthMat_subtopics)

#AuthMat0sparse <- as(AuthMat0, "sparseMatrix")
rm(AuthMat0_subtopics)

## Anwendung Cosine - akutelle Alternative, da schneller #######################

tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

CosineSparse_subtopics <- proxyC::simil(AuthMat0sparse_subtopics, method = "cosine", drop0 = TRUE)

parallelMap::parallelStop()
toc()

#Dauer: 25.83 Sekunden
# Beim zweiten Mal hat es geringfügig länger gedauert, nämlich 28.63 Sekunden
object.size(CosineSparse_subtopics) ### knapp 1.2 GB !!






