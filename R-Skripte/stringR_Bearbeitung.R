########### Erstellung Items6 mit strinR Package für Item-Tokenization & Word-Mining #########

### wichtige libraries
library(tidyverse) ### wird grundsätzlich eingeladen
library(tictoc) ### zeitmessungen für berechnungen
library(parallel) ### parallele benutzung aller verfügbaren kerne auf laptop
library(parallelMap) ### siehe parallel
library(proxyC) ### berechnung der cosine similarities
library(Matrix) ### abspeichern in speziellem Harwell-Boeing-Format für matrizen

### items daten einlesen
items <- read_csv(file="./Data/items5.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_character(),
  author=col_character(),
  publisher=col_character(),
  main.topic=col_character(),
  subtopics=col_character()
))
head(items, n=20)
glimpse(items)

### erstes aufräumen der items daten --> jeweils mit einzelner sapply() funktion, da dadurch schnellere berechnung als innerhalb der pipe-funktion
### utf8 encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"))
### whitespace von beiden seiten entfernen
items[, 2:6] <- apply(items[, 2:6], MARGIN=2, FUN=function(x) str_trim(string=x,side="both"))
### alle strings in kleinen buchstaben darstellen --> im default "english" eingestellt, allerdings mehr bücher auf deutsch im datensatz enthalten
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_to_lower(string=x, locale="de"))
### komische sonderzeichen ersetzen durch "" 
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""))
head(items, n=20)

### subTopics separat von sonderzeichen befreien
#items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)
## kommas müssen extra angezielt werden zur entfernung
#items$subtopics <- gsub(","," ",items$subtopics)
#head(items, n=20)

### mainTopics und subTopics zusammenführen und subtopics als einzelne variable entfernen
items <- items %>%
  mutate(uniteTopics = paste(main.topic, subtopics, sep =  "")) %>% 
  select(-subtopics)
head(items, n=20)
### funktion definieren, um dopplungen aus maintopics und subtopics in kommender variable 'uniteTopics' zu entfernen
rem_dup_word <- function(x) {
  paste(unique(trimws(unlist(strsplit(x, split=" ", fixed=F, perl=T) ), which="both" ) ), collapse= " " )
}

### neue variable items_bearb definieren (damit bei fehler nicht immer skript neu gestartet werden muss!)
items_bearb <- items

### oben definierte funktion mittels sapply() anwenden, die ursprünglichen namen dabei nicht extra als attribut abspeichern
items_bearb <- items_bearb %>%
  mutate(
    newTopics = unlist(sapply(uniteTopics, FUN=rem_dup_word, USE.NAMES=F))) %>%
  select(-uniteTopics) %>%
  rename(uniteTopics = newTopics, mainTopic = main.topic)
head(items_bearb, n=20)
str(items_bearb)

items <- items_bearb
head(items, n=20)
rm(items_bearb, rem_dup_word)

### neues csv "items7" schreiben
write_csv(items, file="./Data/items6.csv", col_names=T)
### testweise einlesen
items2 <- read_csv(file="./Data/items6.csv", col_names=T, col_types=cols(
   itemID=col_factor(),
   title=col_character(),
   author=col_character(),
   publisher=col_character(),
   mainTopic=col_factor(),
   uniteTopics=col_factor()
 ))
 head(items2, n=20) # hat Geklappt!

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

### datensatz bereinigung hier auch anwenden, allerdings nur auf spalten 2 bis 4, mainTopics und subTopics werden extra behandelt
### utf8 überall encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"))
### whitespace von beiden seiten entfernen
items[, 2:4] <- apply(items[, 2:4], MARGIN=2, FUN=function(x) str_trim(string=x,side="both"))
### alle strings in kleinen buchstaben darstellen
items[, 2:4] <- sapply(X=items[,2:4], FUN=function(x) str_to_lower(string=x, locale="de")) # locale="english" im default...besser als Deutsch?
### komische sonderzeichen ersetzen durch "" 
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""))
#items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:alnum:]]", replacement=" "))
head(items, n=20)

### subTopics von sonderzeichen befreien --> KOMMAS BLEIBEN!
items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)
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

#### funktion definieren, um einzelne topics bzw subtopics in extra variablen aufzuspalten
split_into_multiple <- function(column, pattern = ",", into_prefix) {
  cols <- str_split_fixed(column, pattern, n=Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.data.frame(cols)
  # jeweilige columns benannt mit "columnName"_Zahl 
  # where m = Anzahl an Columns für Zahlenbenennung
  m <- dim(cols)[2]
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

### mittels bind_cols unsere definierte funktion anwenden, einzelne (sub-)topics werden separat aneinander geheftet und mit jeweiligem prefix _1,...,_m als neue variablen definiert
items_bearb2 <- items_bearb %>% 
  bind_cols(split_into_multiple(.$uniteTopics, ",", "topic")) %>% 
  # nur Spalten wählen mit Prefix "_"
  select(author, starts_with("topic_"))
head(items_bearb2, n=20) #### nur 33 spalten variablen?
str(items_bearb2)

### erst einzelne topic variablen im long-format anordnen, sodass jeder autor mehrmals untereinandern mit seinen jeweils vorhandenen topics erscheint
### danach das dataframe ins weite format übersetzen und dazu die häufigkeit des auftretens n() als entscheidenden wert definieren
### somit wird pro topic die häufigkeit des auftretens des jeweiligen autors in diesem topic ausgegeben
items_bearb3 <- items_bearb2 %>%
  gather(topic_1:topic_32, key=numberTopic, value=topic, -author, na.rm=T) %>% # author als linke wichtige entscheidungsvariable lassen
  group_by(author, topic) %>% 
  summarise(n=n()) %>%
  spread(key=topic, value=n) ### sagenhafte 1828 variablen
str(items_bearb3)
head(items_bearb3, n=20)
rm(items, items_bearb, items_bearb2, rem_dup_word, split_into_multiple) # funktionen und eingelese items datensätze wieder löschen

##############cosine matrix erstellen zwischen autoren und belegung der jeweiligen topics ##########
# matrix aufbauen, mit den dimensionen entsprechend der anzahl der autoren und subtopis
AuthMat_subtopics <- items_bearb3 #%>% 

# Autorennamen abspeichern, da diese später als rownames eingefügt werden
Authors_subtopics <- AuthMat_subtopics$author

# welchen typen haben die einträge unter den variablen --> character einträge!!
apply(X=AuthMat_subtopics, MARGIN=2, FUN=class)

### character werte als integer neu kodieren und in der matrix abspeichern mittels der apply() funktion
AuthMat_subtopics <- apply(X=AuthMat_subtopics[, 2:ncol(AuthMat_subtopics)], MARGIN=2, FUN=as.integer)

# überprüfung, ob die transformation funktioniert hat
apply(X=AuthMat_subtopics, MARGIN=2, FUN=class)

# nun befindet sich die matrix in der gewünschten formatierung:
# nn den zeilen stehen die autoren und in den spalten die mainTopics
# dazu kommt, dass die Spalten nun auch alle numerisch sind, folglich können
# darauf nun similarity-algorithmen angewendet werden.

# grundsätzlich bieten sich hierfür folgende drei an:
# 1. Pearson Correlation
# 2. Jaccard Coefficient
# 3. Cosine Similarity

# während für die Pearson Correlation schon eine implementierung in {stats}
# vorhanden ist, benötigen wir für die anderen beiden ein extra package.
# hierfür wählen wir das package "proxyC" da wir mit diesem sowohl den Jaccard
# Coefficient berechnen können, als auch die Cosine Similarity

# Für große Matritzen
# install.packages("proxyC")
# library(proxyC)

## zur sicherheit wieder eine neue dummy-matrix erstellen
AuthMat0_subtopics <- AuthMat_subtopics
### zum schnelleren berechnen als na's als "0"-integer abspeichern
AuthMat0_subtopics[is.na(AuthMat0_subtopics)] <- 0
### hier auch nochmal extra den mode der mazrix als integer definieren
mode(AuthMat0_subtopics) <- "integer"

# zeilennamen der matrizen durch due namnen der autoren ersetzen
rownames(AuthMat_subtopics) <- Authors_subtopics
rownames(AuthMat0_subtopics) <- Authors_subtopics

### sparse matrix erstellen, da sehr recheneffizient und wir sowieso nur integer-werte in der matrix haben
AuthMat0sparse_subtopics <- as(AuthMat0_subtopics, "sparseMatrix")

rm(AuthMat0_subtopics, AuthMat_subtopics, Authors_subtopics)

################ Anwendung von proxyC ################################
### anwendung cosine - akutell beste alternative, da schneller

# dauer der berechnung messen
tictoc::tic("Dauer Cosine mit proxyC und drop0 = TRUE")

### parallele berechnung durch nutzung aller verfügbaren kerne sicherstellen
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

### cosine similarities bestimmen und die "0"en rauswerfen, sodass sie nichts verzerren
CosineSparse_subtopics <- proxyC::simil(AuthMat0sparse_subtopics, method = "cosine", drop0 = TRUE)

parallelMap::parallelStop()
tictoc::toc() # etwa 40 sekunden laufzeit bei 8 GB RAM ohne weitere anwendungen nebenbei

### wie groß ist unsere datei?
object.size(CosineSparse_subtopics) ### knapp 1.2 GB !!

### Matrix abspeichern im Harwell-Boeing-Format
#Matrix::writeMM(obj=CosineSparse_subtopics, file="CosineSparse_subtopics.mtx")

########### items-uniteTopics-Matrix nach gleichem Schema erstellen ######
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

#### funktion um einzelne topics in extra variablen aufzuspalten
split_into_multiple <- function(column, pattern = ",", into_prefix) {
  cols <- str_split_fixed(column, pattern, n=Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.data.frame(cols)
  # jeweilige columns benannt mit "columnName"_Zahl 
  # where m = Anzahl an Columns für Zahlenbenennung
  m <- dim(cols)[2]
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

items_bearb2 <- items_bearb %>% 
  bind_cols(split_into_multiple(.$uniteTopics, ",", "topic")) %>% 
  # nur Spalten wählen mit Prefix "_"
  select(itemID, starts_with("topic_"))

items_bearb3 <- items_bearb2 %>%
  gather(topic_1:topic_32, key=numberTopic, value=topic, -itemID, na.rm=T) %>% # author als linke wichtige entscheidungsvariable lassen
  group_by(itemID, topic) %>% 
  summarise(n=n()) %>%
  spread(key=topic, value=n)
str(items_bearb3)
head(items_bearb3, n=20)
rm(items, items_bearb, items_bearb2, rem_dup_word, split_into_multiple) # funktionen und eingelese items datensätze wieder löschen

##############cosine matrix erstellen zwischen autoren und belegung der jeweiligen topics ##########
### matrix aufbauen, mit den dimensionen entsprechend der anzahl der item id's und der subTopics
itemMat_subtopics <- items_bearb3 #%>% 

### item id's abspeichern, da diese später als rownames eingefügt werden
items_subtopics <- itemMat_subtopics$itemID

#apply(X=itemMat_subtopics, MARGIN=2, FUN=class)

itemMat_subtopics <- apply(X=itemMat_subtopics[, 2:ncol(itemMat_subtopics)], MARGIN=2, FUN=as.integer)

### überprüfung, ob transformation funktioniert hat
apply(X=itemMat_subtopics, MARGIN=2, FUN=class) # top

### na's als "0" und ansosnte auch der mode als integer
itemMat0_subtopics <- itemMat_subtopics
itemMat0_subtopics[is.na(itemMat0_subtopics)] <- 0
mode(itemMat0_subtopics) <- "integer"

### rownames der items id's einfügen
rownames(itemMat_subtopics) <- items_subtopics
rownames(itemMat0_subtopics) <- items_subtopics

### sparse matrix aufbauen
itemMat0sparse_subtopics <- as(itemMat0_subtopics, "sparseMatrix")

rm(itemMat0_subtopics, itemMat_subtopics, items_subtopics, items_bearb3)

############ Anwendung von proxyC ##############

### Anwendung Cosine

tictoc::tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

itemsCosineSparse_subtopics <- proxyC::simil(itemMat0sparse_subtopics, method = "cosine", drop0 = TRUE)

parallelMap::parallelStop()
tictoc::toc() # etwa 40 sekunden laufzeit bei 8 GB RAM ohne weitere anwendungen nebenbei

object.size(itemsCosineSparse_subtopics) ### knapp 4.2 GB

#### matrix abspeichern
#Matrix::writeMM(obj=itemsCosineSparse_subtopics, file="itemsCosineSparse_subtopics.mtx")
