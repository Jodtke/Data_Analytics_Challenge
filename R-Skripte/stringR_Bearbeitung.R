########### Erstellung Items-Datensatz mit entscheidenden Variablen für Berechnungen und Gewichtungen ##########
########### sowie der Sparse-Matrizes der Author-Topic-Similarities sowie der Item-Topic-Similarities ##########

### notwendige Libraries installieren
# install.packages("tidyverse")
# install.packages("proxyC")
# install.packages("Matrix")
# install.packages("parallel")
# install.packages("parallelMap")
# install.packages("tictoc")

### Libraries laden
library(tidyverse) ### wird grundsätzlich eingeladen
library(tictoc) ### Zeitmessungen für Berechnungen
library(parallel) ### parallele Benutzung aller verfügbaren Kerne auf Laptop
library(parallelMap) ### siehe parallel
library(proxyC) ### Berechnung der Cosine-Similarities
library(Matrix) ### abspeichern in speziellem Harwell-Boeing-Format für Matrizen

### Items-Daten einlesen
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

### erstes Aufräumen der items Daten --> jeweils mit einzelner sapply() Funktion, da schnellere Berechnung als innerhalb einer pipe-Funktion
## UTF8 encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"), USE.NAMES=F )
## Whitespace von beiden Seiten entfernen, Topics auslassen
items[, 2:4] <- apply(items[, 2:4], MARGIN=2, FUN=function(x) str_trim(string=x,side="both") )
## alle Strings in kleinen Buchstaben darstellen --> im default "english" eingestellt, allerdings mehr Bücher auf deutsch im Datensatz enthalten, Topics auslassen
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_to_lower(string=x, locale="de"), USE.NAMES=F )
## Sonderzeichen und sprachliche Eigenheiten ersetzen durch "", Topics auslassen
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""), USE.NAMES=F ) ### WICHTIG: Replacement muss mit Crawler-Daten übereinstimmen, sonst kein Merging möglich!!
head(items, n=20)

### subTopics separat von Sonderzeichen befreien
items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)
## Kommas müssen extra angezielt werden zur Entfernung
items$subtopics <- gsub(","," ",items$subtopics)
head(items, n=20)

### mainTopics und subTopics zusammenführen zu uniteTopics und subTopics als einzelne Variable entfernen
items <- items %>%
  mutate(
    uniteTopics = paste(main.topic, subtopics, sep=" ")
    ) %>% 
  select(-subtopics)
head(items, n=20)

### Funktion definieren, um Dopplungen aus mainTopics und subTopics in uniteTopics zu entfernen
rem_dup_word <- function(x) {
  x <- tolower(x) ## auch die Topics gemeinschaftlich in kleinen Buchstaben speichern
  paste(unique(trimws(unlist(strsplit(x, split=" ", fixed=F, perl=T) ), which="both" ) ), collapse= " " ) ## von beiden Seiten Whitespace entfernen, Perl-kompatible Regular Expressions zugelassen
}

### oben definierte Funktion mittels sapply() anwenden 
items_bearb <- items %>% ## neue Variable items_bearb definieren, damit bei Fehler nicht immer das Skript neu gestartet werden muss 
  mutate(
    newTopics = unlist(sapply(uniteTopics, FUN=rem_dup_word, USE.NAMES=F) ) ## die ursprünglichen Namen dabei nicht extra als Attribut abspeichern
    ) %>% 
  select(-uniteTopics) %>%
  rename(uniteTopics = newTopics, mainTopic = main.topic) %>%
  mutate(
    mainTopic = tolower(mainTopic)
    )
head(items_bearb, n=20)
str(items_bearb)

### items einfach überschreiben mit items_bearb, um Veränderungen abspeichern zu können
items <- items_bearb
head(items, n=20)
rm(items_bearb, rem_dup_word)

### neues csv "items6" schreiben
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
rm(items2)

############################### Crawler-Daten aufbereiten ####################################

### Import der Crawler-Daten
FCD <- readRDS("./Data/ersteCrawlerDaten.rds")

### Listenelemente innerhalb von Listenelementen als Tibble extrahieren
FCD_tibble <- as_tibble(do.call("rbind", lapply(FCD, '[', c(1, 15)) ) )

### Anzahl der NAs
sum(is.na(FCD_tibble$Beschreibung) ) # 14061 Items haben keine Klappentexte

### Tibble aus den Titeln und Beschreibungen der Klappentexte erstellen
FCD_tibble <- FCD_tibble %>% 
  rename(title = Titel) %>% 
  mutate(
    title = unlist(title), ## muss "entlistet" werden
    Beschreibung = unlist(Beschreibung) ## siehe oben
    )
rm(FCD)
head(FCD_tibble, n=20)

### Crawler-Daten an items anpassen, Namen nicht als Attribute übernehmen
## UTF8 encodieren
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_conv(string=x, encoding="UTF-8"), USE.NAMES=F)
## Whitespace von beiden Seiten entfernen
FCD_tibble[, 1:2] <- apply(X=FCD_tibble[, 1:2], MARGIN=2, FUN=function(x) str_trim(string=x, side="both") )
## alle Strings in kleinen Buchstaben darstellen
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_to_lower(string=x, locale="de"), USE.NAMES=F) # locale="english" im default...besser als Deutsch?
## Sonderzeichen ersetzen durch "" 
FCD_tibble[, 1:2] <- sapply(X=FCD_tibble[, 1:2], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""), USE.NAMES=F)
head(FCD_tibble, n=20)

### als eigene csv-Datei abspeichern
write_csv(FCD_tibble, file="./Data/KlappentexteUndTitel.csv", col_names=T)

### testweise einlesen
 FCD_tibble2 <- read_csv(file="./Data/KlappentexteUndTitel.csv", col_names=T, col_types=cols(
    title=col_character(),
    Beschreibung=col_character()
    ))
head(FCD_tibble2, n=20) # hat Geklappt!
rm(FCD_tibble2)


############## DataFrame erstellen im longer-Format für Authors und Topics als Keys ##############

### items für Bearbeitung neu und extra einlesen, da Kommas zwischen den subTopics NICHT entfernt werden
## items aber NICHT ALS NEUE ITEM DATEI ABSPEICHERN!
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

### Datensatz Bereinigung hier auch anwenden, mainTopics und subTopics werden wieder extra behandelt
## UTF8 encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"), USE.NAMES=F)
## Whitespace von beiden Seiten entfernen
items[, 2:4] <- apply(items[, 2:4], MARGIN=2, FUN=function(x) str_trim(string=x,side="both") )
## alle Strings in kleinen Buchstaben darstellen
items[, 2:4] <- sapply(X=items[,2:4], FUN=function(x) str_to_lower(string=x, locale="de"), USE.NAMES=F )
## Sonderzeichen ersetzen durch ""
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""), USE.NAMES=F) ## ACHTUNG: Replacement muss identisch sein zu Crawler-Daten

head(items, n=20)

### subTopics von Sonderzeichen befreien --> KOMMAS BLEIBEN!
items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)

head(items, n=20)

### mainTopics und subTopics zusammenführen und subTopics als einzelne Variable entfernen
items <- items %>%
  mutate(uniteTopics = paste(main.topic, subtopics, sep = ",")  ## ACHTUNG: Komma nur für Merge mit Crawler-Daten drin!!
         ) %>%
  select(-subtopics)

head(items, n=20)

### Dopplungen aus mainTopics und subTopics in Variable 'uniteTopics' entfernen und mainTopics in Kleinbuchstaben
rem_dup_word <- function(x) {
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x, split=" ", fixed=F, perl=T) ), which="both" ) ), collapse= " " )
}

### neue Variable items_bearb definieren, damit bei Fehler nicht immer Skript neu geladen werden muss
items_bearb <- items %>%
  mutate(
    newTopics = unlist(sapply(uniteTopics, FUN=rem_dup_word, USE.NAMES=F) )
    ) %>%
  select(-uniteTopics) %>%
  rename(uniteTopics = newTopics, mainTopic = main.topic) %>%
  mutate(
    mainTopic = tolower(mainTopic)
    )

head(items_bearb, n=20)
str(items_bearb)

### items überschreiben mit items_bearb
items <- items_bearb
head(items, n=20)

#### Funktion definieren, um einzelne Topics in uniteTopics in extra Variablen aufzuspalten
split_into_multiple <- function(column, pattern = ",", into_prefix) { ## Komma zwischen den Variablen entscheidendes Muster für Spaltung
  cols <- str_split_fixed(column, pattern, n=Inf)
  cols[which(cols == "")] <- NA ## NA's statt leere Zellen
  cols <- as.data.frame(cols) ## als DataFrame abspeichern (als Tibble hat komischerweise immer eine Fehlermeldung gegeben?!)
  m <- dim(cols)[2] ## m = Anzahl an Columns für Zahlenbenennung
  names(cols) <- paste(into_prefix, 1:m, sep = "_") ## jeweilige Columns benannt mit "columnName"_Zahl 
  return(cols)
}

### mittels bind_cols unsere definierte Funktion anwenden und mit jeweiligem prefix _1,...,_m als neue Variablen definiert
items_bearb2 <- items_bearb %>% 
  bind_cols(split_into_multiple(.$uniteTopics, ",", "topic")) %>% ## einzelne Topics werden separat aneinander geheftet
  select(author, starts_with("topic_")) ## nur Spalten wählen mit Prefix "_" 

str(items_bearb2)
head(items_bearb2, n=20)

### einmal ins lange Format und wieder zurück...ins weite Format
## erst einzelne Topics im langen Format anordnen, sodass jeder Autor mehrmals untereinander mit seinen jeweiligen Topics erscheint
## danach das DataFrame ins weite Format übersetzen und dazu die Häufigkeit des auftretens n() als spread-Wert definieren
## somit wird pro Topic die Häufigkeit des Auftretens des jeweiligen Autors in diesem Topic ausgegeben
items_bearb3 <- items_bearb2 %>%
  gather(topic_1:topic_32, key=numberTopic, value=topic, -author, na.rm=T) %>% ## Author als linke wichtige Entscheidungsvariable lassen
  group_by(author, topic) %>% 
  summarise(
    n=n()
    ) %>%
  spread(key=topic, value=n) ## sagenhafte 1850 variablen

str(items_bearb3)
head(items_bearb3, n=20)
rm(items, items_bearb, items_bearb2, rem_dup_word, split_into_multiple) ## Funktionen und eingelesene items wieder löschen

############## Cosine-Matrix erstellen zwischen Autoren und Belegung der jeweiligen Topics ####################

### Matrix aufbauen, mit den Dimensionen entsprechend der Anzahl der Autoren und Subtopis
AuthMat_subtopics <- items_bearb3

### Autorennamen abspeichern, da diese später als Rownames eingefügt werden
Authors_subtopics <- AuthMat_subtopics$author

### welchen Typen haben die Einträge unter den Variablen
apply(X=AuthMat_subtopics, MARGIN=2, FUN=class) ## characters!!

### character Werte als integer neu kodieren und in der Matrix abspeichern mittels der apply() Funktion
AuthMat_subtopics <- apply(X=AuthMat_subtopics[, 2:ncol(AuthMat_subtopics)], MARGIN=2, FUN=as.integer)

### Überprüfung, ob die Transformation funktioniert hat
apply(X=AuthMat_subtopics, MARGIN=2, FUN=class) ## hat geklappt!

## nun befindet sich die Matrix in der gewünschten Formatierung:
## in den Zeilen stehen die Autoren und in den Spalten die mainTopics
## dazu kommt, dass die Spalten nun auch alle numerisch sind, folglich können
## darauf nun Similarity-Algorithmen angewendet werden.

### grundsätzlich bieten sich hierfür folgende drei an:
## 1. Pearson Correlation
## 2. Jaccard Coefficient
## 3. Cosine Similarity

## während für die Pearson Correlation schon eine Implementierung in {stats}
## vorhanden ist, benötigen wir für die anderen beiden ein extra Package.
## hierfür wählen wir das Package "proxyC" da wir mit diesem sowohl den Jaccard Coefficient
##  berechnen können, als auch die Cosine-Similarity

### zur Sicherheit wieder eine neue Dummy-Matrix erstellen
AuthMat0_subtopics <- AuthMat_subtopics

### zum schnelleren Berechnen als NA's als "0"-integer abspeichern
AuthMat0_subtopics[is.na(AuthMat0_subtopics)] <- 0

### hier auch nochmal extra den Mode der Matrix als integer definieren
mode(AuthMat0_subtopics) <- "integer"

### Zeilennamen der Matrizen durch die Namen der Autoren ersetzen
rownames(AuthMat_subtopics) <- Authors_subtopics
rownames(AuthMat0_subtopics) <- Authors_subtopics

### Sparse-Matrix erstellen, da sehr recheneffizient und wir sowieso nur integer Werte in der Matrix haben
AuthMat0sparse_subtopics <- as(AuthMat0_subtopics, "sparseMatrix")

### unwichtige Variablen wieder löschen
rm(AuthMat0_subtopics, AuthMat_subtopics, Authors_subtopics)

### Anwendung Cosine, da 'schnellste' Alternative
## Dauer der Berechnung messen
tictoc::tic("Dauer Cosine mit proxyC und drop0 = TRUE")

### parallele Berechnung durch Nutzung aller verfügbaren Kerne sicherstellen
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

### Cosine-Similarities bestimmen und die "0"en rauswerfen, sodass sie nichts verzerren
CosineSparse_subtopics <- proxyC::simil(AuthMat0sparse_subtopics, method = "cosine", drop0 = TRUE)

parallelMap::parallelStop()
tictoc::toc() ## etwa 40 Sekunden Laufzeit bei 8 GB RAM ohne weitere Anwendungen nebenbei

### wie groß ist unsere Datei?
object.size(CosineSparse_subtopics) ## knapp 1.2 GB !!

### Matrix abspeichern im Harwell-Boeing-Format
# Matrix::writeMM(obj=CosineSparse_subtopics, file="CosineSparse_subtopics.mtx")

############### items-uniteTopics-Matrix nach gleichem Schema erstellen ################
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

### Datensatz Bereinigung hier auch anwenden, mainTopics und subTopics werden wieder extra behandelt
## UTF8 encodieren
items[, 2:6] <- sapply(X=items[, 2:6], FUN=function(x) str_conv(string=x, encoding="UTF-8"), USE.NAMES=F)
## Whitespace von beiden Seiten entfernen
items[, 2:4] <- apply(items[, 2:4], MARGIN=2, FUN=function(x) str_trim(string=x,side="both") )
## alle Strings in kleinen Buchstaben darstellen
items[, 2:4] <- sapply(X=items[,2:4], FUN=function(x) str_to_lower(string=x, locale="de"), USE.NAMES=F )
## Sonderzeichen ersetzen durch ""
items[, 2:4] <- sapply(X=items[, 2:4], FUN=function(x) str_replace_all(string=x, pattern="[[:punct:]]", replacement=""), USE.NAMES=F) ## ACHTUNG: Replacement muss identisch sein zu Crawler-Daten

head(items, n=20)

### subTopics von Sonderzeichen befreien --> KOMMAS BLEIBEN!
items$subtopics <- gsub("\\[|\\]", "", items$subtopics, perl=T)

head(items, n=20)

### mainTopics und subTopics zusammenführen und subTopics als einzelne Variable entfernen
items <- items %>%
  mutate(uniteTopics = paste(main.topic, subtopics, sep = ",")  ## ACHTUNG: Komma nur für Merge mit Crawler-Daten drin!!
  ) %>%
  select(-subtopics)

head(items, n=20)

### Dopplungen aus mainTopics und subTopics in Variable 'uniteTopics' entfernen und mainTopics in Kleinbuchstaben
rem_dup_word <- function(x) {
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x, split=" ", fixed=F, perl=T) ), which="both" ) ), collapse= " " )
}

### neue Variable items_bearb definieren, damit bei Fehler nicht immer Skript neu geladen werden muss
items_bearb <- items %>%
  mutate(
    newTopics = unlist(sapply(uniteTopics, FUN=rem_dup_word, USE.NAMES=F) )
  ) %>%
  select(-uniteTopics) %>%
  rename(uniteTopics = newTopics, mainTopic = main.topic) %>%
  mutate(
    mainTopic = tolower(mainTopic)
  )

head(items_bearb, n=20)
str(items_bearb)

### items überschreiben mit items_bearb
items <- items_bearb

head(items, n=20)

#### Funktion definieren, um einzelne Topics in uniteTopics in extra Variablen aufzuspalten
split_into_multiple <- function(column, pattern = ",", into_prefix) { ## Komma zwischen den Variablen entscheidendes Muster für Spaltung
  cols <- str_split_fixed(column, pattern, n=Inf)
  cols[which(cols == "")] <- NA ## NA's statt leere Zellen
  cols <- as.data.frame(cols) ## als DataFrame abspeichern (als Tibble hat komischerweise immer eine Fehlermeldung gegeben?!)
  m <- dim(cols)[2] ## m = Anzahl an Columns für Zahlenbenennung
  names(cols) <- paste(into_prefix, 1:m, sep = "_") ## jeweilige Columns benannt mit "columnName"_Zahl 
  return(cols)
}

### mittels bind_cols unsere definierte Funktion anwenden und mit jeweiligem prefix _1,...,_m als neue Variablen definiert
items_bearb2 <- items_bearb %>% 
  bind_cols(split_into_multiple(.$uniteTopics, ",", "topic")) %>% ## einzelne Topics werden separat aneinander geheftet
  select(itemID, starts_with("topic_")) ## nur Spalten wählen mit Prefix "_"

str(items_bearb2)
head(items_bearb2, n=20)

### einmal ins lange Format und wieder zurück...ins weite Format
## erst einzelne Topics im langen Format anordnen, sodass jede itemID mehrmals untereinander mit seinen jeweiligen Topics erscheint
## danach das DataFrame ins weite Format übersetzen und dazu die Häufigkeit des auftretens n() als spread-Wert definieren
## somit wird pro Topic die Häufigkeit des Auftretens der jeweiligen itemID in diesem Topic ausgegeben
items_bearb3 <- items_bearb2 %>%
  gather(topic_1:topic_32, key=numberTopic, value=topic, -itemID, na.rm=T) %>% ## itemID als linke wichtige Entscheidungsvariable lassen
  group_by(itemID, topic) %>% 
  summarise(
    n=n()
  ) %>%
  spread(key=topic, value=n) ## sagenhafte 1850 variablen

str(items_bearb3)
head(items_bearb3, n=20)

rm(items, items_bearb, items_bearb2, rem_dup_word, split_into_multiple) ## Funktionen und eingelesene items wieder löschen

############## Cosine-Matrix erstellen zwischen itemID und Belegung der jeweiligen Topics #############

### Matrix aufbauen, mit den Dimensionen entsprechend der Anzahl der itemID und der subTopics
itemMat_subtopics <- items_bearb3

### itemID abspeichern, da diese später als Rownames eingefügt werden
items_subtopics <- itemMat_subtopics$itemID

itemMat_subtopics <- apply(X=itemMat_subtopics[, 2:ncol(itemMat_subtopics)], MARGIN=2, FUN=as.integer)

### Überprüfung, ob Transformation funktioniert hat
apply(X=itemMat_subtopics, MARGIN=2, FUN=class) ## hat geklappt

### NA's als "0" und ansonsten auch der Mode als integer
itemMat0_subtopics <- itemMat_subtopics
itemMat0_subtopics[is.na(itemMat0_subtopics)] <- 0
mode(itemMat0_subtopics) <- "integer"

### Rownames der itemID's einfügen
rownames(itemMat_subtopics) <- items_subtopics
rownames(itemMat0_subtopics) <- items_subtopics

### Sparse-Matrix aufbauen
itemMat0sparse_subtopics <- as(itemMat0_subtopics, "sparseMatrix")

### unwichtige Variablen löschen
rm(itemMat0_subtopics, itemMat_subtopics, items_subtopics, items_bearb3)

### Anwendung Cosine
tictoc::tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

itemsCosineSparse_subtopics <- proxyC::simil(itemMat0sparse_subtopics, method = "cosine", drop0 = TRUE)

parallelMap::parallelStop()
tictoc::toc() # etwa 40 Sekunden Laufzeit bei 8 GB RAM ohne weitere Anwendungen nebenbei

### Größe der Datei
object.size(itemsCosineSparse_subtopics) ### knapp 4.2 GB

#### matrix abspeichern
# Matrix::writeMM(obj=itemsCosineSparse_subtopics, file="itemsCosineSparse_subtopics.mtx")

###################### Ende ########################
