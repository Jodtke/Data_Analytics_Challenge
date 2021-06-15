# Hybrid recommender System

# Working Directory Definieren

getwd()
setwd("C:/Users/Rolf/Desktop/WFI - Unterlagen/Master/4. Semester/Data Analytics Challenge/Data_Analytics_Challenge")

# Packages installieren

# install.packages("tidyverse")
# install.packages("tictoc")
# install.packages("parallelMap")
# install.packages("parallel")
# install.packages("proxyC")
# install.packages("lsa")
# install.packages("tm")

# Packages laden

library(tidyverse)
library(tictoc)
library(parallelMap)
library(parallel)
library(proxyC)
library(tm)

### DatensÃ¤tze laden

# Datensatz mit Item-Informationen
items7 <- read.csv("./Data/items7.csv", encoding = "UTF-8")

# Datensatz mit Transaktionsdaten
transactions <- as_tibble(read.csv("./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F))

# Crawler-Daten mit Klappentexten
FCD_tibble <- as_tibble(read.csv("./Data/KlappentexteUndTitel.csv", encoding = "UTF-8"))

# Evaluations-Daten
evaluation <- read.csv("./Data/evaluation.csv")

### DatensÃ¤tze transformieren und ergÃ¤nzende erstellen

### Cosine Similarity Matrix erstellen (basierend auf Subtopics)

# Erstelle Similarity-Matrix, welche die Cosine Similarity und somit die
# Ãhnlichkeit zwischen den Autoren berechnet.
# Dabei wird BerÃ¼cksichtigt, wie viele BÃ¼cher die Autoren im jeweiligen
# Main Topic und Sub Topic geschrieben haben

# Imporiteren der AuthMat_subtopics aus anderem Skript
AuthMat <- read.csv("./Data/AuthMat_subtopics.csv", check.names = FALSE)

# Autorennamen abspeichern, da diese spÃ¤ter als rownames eingefÃ¼gt werden
Authors <- AuthMat$author

# Spalten in integer konvertieren
AuthMat <- apply(AuthMat[2:ncol(AuthMat)],2,as.integer)

# Die NA's durch Nullen ersetzen
AuthMat[is.na(AuthMat)] <- 0

# Die Matrix in Integer konvertieren um Speicherplatz zu sparen, ist aktuell numeric
mode(AuthMat) <- "integer"

# Defintion der Zeilennamen mit den Namen der Autoren
rownames(AuthMat) <- Authors

# Die Matrix in eine sparse Matrix konvertieren, um Speicherplatz zu sparen
AuthMatSparse <- as(AuthMat, "sparseMatrix")

# Basierend auf dieser sparse Matrix die Cosine Similarity zwischen den Autoren berechnen
tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelStartSocket(cpus = detectCores())

CosineSparse <- proxyC::simil(AuthMatSparse, method = "cosine", drop0 = TRUE)

parallelStop()
toc()

# Nicht mehr benötigte Datensätze löschen, um Speicherplatz zu sparen
rm(Authors, AuthMat, AuthMatSparse)

### Erstellung Item-Matrix, welche Ausprägung in entsprechendem Subtopic enthält

# Imporiteren der itemMat_subtopics aus anderem Skript
itemMat <- read.csv("./Data/itemMat_subtopics.csv", check.names = FALSE)

# ItemID's abspeichern, da diese später als rownames eingefügt werden
itemIDs <- as.character(itemMat$itemID)

# Spalten in integer konvertieren
itemMat <- apply(X=itemMat[, 2:ncol(itemMat)], MARGIN=2, FUN=as.integer)

# Die NA's durch Nullen ersetzen
itemMat[is.na(itemMat)] <- 0

# Die Matrix in Integer konvertieren um Speicherplatz zu sparen, ist aktuell numeric
mode(itemMat) <- "integer"

# Defintion der Zeilennamen mit den ItemID's
rownames(itemMat) <- itemIDs

# Die Matrix in eine sparse Matrix konvertieren, um Speicherplatz zu sparen
itemMatSparse <- as(itemMat, "sparseMatrix")

# Nicht mehr benötigte Datensätze löschen, um Speicherplatz zu sparen
rm(itemIDs, itemMat)

### joined_oR erstellen

# ZusammenfÃ¼gen der Transaktions- und Item-Daten, welche als Ausgangspunkt
# zur Gewinnung weiterer Features dienen
joined_oR <- left_join(transactions, items7, by = "itemID")

# Erstellung von dup.ses, um im spÃ¤teren Verlauf eine Art ARM anwenden zu kÃ¶nnen
dup.ses <- joined_oR[joined_oR$sessionID %in%
                       joined_oR$sessionID[duplicated(joined_oR$sessionID)],]

# Erstellung der Variable inDupSes, welche darÃ¼ber informiert, ob sich ein Item
# in Dup.Ses befindet oder nicht
inDupSes <- dup.ses %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(inDupSes = TRUE)

# Erstellung der Variable inTransactions, welche darÃ¼ber informiert, ob sich ein 
# Item transactions befindet oder nicht 
itemsInTrans <- joined_oR %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(inTransactions = TRUE)

# Erstellung der Variable clicked, welche darÃ¼ber informiert, ob ein Item 
# mindestens einmal angeklickt wurde oder nicht 
clicked <- joined_oR %>% 
  filter(click > 0) %>%
  select(itemID) %>% 
  unique() %>% 
  mutate(clicked = TRUE)

# Erstellung der Variable basket, welche darÃ¼ber informiert, ob ein Item 
# mindestens einmal in den Warenkorb gelegt wurde oder nicht
basket <- joined_oR %>%
  filter(basket > 0) %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(basket = TRUE)

# Erstellung der Variable ordered, welche darÃ¼ber informiert, ob ein Item 
# mindestens einmal gekauft wurde oder nicht
ordered <- joined_oR %>%
  filter(order > 0) %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(ordered = TRUE)

### Reduzierung der Crawler-Daten

# Da diverse BÃ¼cher mit dem identischen Titel vorliegen, die Item-Daten mit den
# Crawler-Daten jedoch Ã¼ber die Titel gejoint werden, mÃ¼ssen zunÃ¤chst alle
# Duplikate basierend auf den Titeln aussoritert werden.
FCD_tibble <- FCD_tibble %>% distinct(title, .keep_all = TRUE)
# ACHTUNG: Hier besteht immer noch Verbesserungspotential!
# Aktuell wird ja einfach nur das erste Item beibehalten, alle nachfolgenden
# werden aussortiert. Dabei kann es ja aber durchaus passieren, dass das erste
# Item keinen Klappentext hatte, die Duplikate jedoch schon. So gehen die
# Klappentexte verloren!

### Erstellung des tibbles totalInfo

# Erstelle tibble totalInfo, welches folgende Informationen enthÃ¤lt:
# - itemID
# - titel
# - autor
# - publisher
# - main topic
# - unite topic (main topic + sub topics)
# - inTransactions (TRUE/FALSE)
# - clicked (TRUE/FALSE)
# - basket (TRUE/FALSE)
# - ordered (TRUE/FALSE)
# - inDupSes (TRUE/FALSE)
# - Beschreibung
# - titleUndBeschreibung (titel + Beschreibung)

# VerknÃ¼pfung items7 mit itemsInTrans Ã¼ber die itemID
totalInfo <- items7 %>% left_join(itemsInTrans, by = "itemID")
# Ãberschreibung der NA mit FALSE
totalInfo$inTransactions[is.na(totalInfo$inTransactions)] <- FALSE

# VerknÃ¼pfung von totalInfo mit clicked Ã¼ber die itemID
totalInfo <- totalInfo %>% left_join(clicked, by = "itemID")
# Ãberschreibung der NA mit FALSE
totalInfo$clicked[is.na(totalInfo$clicked)] <- FALSE

# VerknÃ¼pfung von totalInfo mit basket Ã¼ber die itemID
totalInfo <- totalInfo %>% left_join(basket, by = "itemID")
# Ãberschreibung der NA mit FALSE
totalInfo$basket[is.na(totalInfo$basket)] <- FALSE

# VerknÃ¼pfung von totalInfo mit ordered Ã¼ber die itemID
totalInfo <- totalInfo %>% left_join(ordered, by = "itemID")
# Ãberschreibung der NA mit FALSE
totalInfo$ordered[is.na(totalInfo$ordered)] <- FALSE

# VerknÃ¼pfung von totalInfo mit inDupSes Ã¼ber die itemID
totalInfo <- totalInfo %>% left_join(inDupSes, by = "itemID")
# Ãberschreibung der NA mit FALSE
totalInfo$inDupSes[is.na(totalInfo$inDupSes)] <- FALSE

# VerknÃ¼pfung von totalInfo mit der Beschreibung (Klappentext) Ã¼ber den Titel
totalInfo <- totalInfo %>% left_join(FCD_tibble, by = "title")

# Erzeugung der Variable titelUndBeschreibung, um Text Mining zu verbessern
totalInfo <- totalInfo %>% mutate(titleUndBeschreibung = paste(title, Beschreibung, sep = " "))

# Nicht mehr benötigte Datensätze löschen, um Speicherplatz zu sparen
rm(clicked, basket, ordered, inDupSes, itemsInTrans, items7, transactions, joined_oR, FCD_tibble)

### Erstellung von activeAuthorAndTitel

# VerknÃ¼pfung des Evaulation-Tibbles mit totalInfo um sÃ¤mtliche Informationen
# Ã¼ber die Ziel-Items zu erhalten

activeAuthorAndTitel <- evaluation %>% left_join(totalInfo, by = "itemID")

# Nicht mehr benötigte Datensätze löschen, um Speicherplatz zu sparen
rm(evaluation)

### Defintion von Funktionen

### Definition der Funktion 'makeDTM'

# Da im weiteren Verlauf Fallunterscheidungen (if-Statements) auftreten, 
# mÃ¼sste der Code mehrmals kopiert und eingefÃ¼gt werden.Durch die Definition 
# einer Funktion sparen wir uns dieses Vorgehen, reduzieren dabei sogar noch 
# Code-Redundanz und minimiern Gefahrenquellen durch manuelle Eingabe

makeDTM <- function(x) {
  
  Corpus <- VCorpus(DataframeSource(x))
  
  Corpus <- Corpus %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>% # ist das sinnvoll?
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removeWords, stopwords("german")) %>%
    tm_map(removeWords, stopwords("french")) %>%
    tm_map(removeWords, stopwords("spanish")) %>% 
    tm_map(removeWords, c("NA","â¢","Â»","Â®", "â", "Â¡", "Â¿", "â"))
  
  # Als nÃ¤chstes wird der Corpus in eine TF-IDF-Matrix Ã¼berfÃ¼hrt:
  DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
  
  return(DTM)
  
}

### Definition der Funktion 'makeAllTitles'

# Da sowohl für die Klappentext-Similarity als auch für die Subtopic-Similarity
# ähnliche Vorselektionen vorgenommen werden, bietet es sich an, eine Funktion
# zu schreiben, die diese übernimmt, um Code-Redundanzen zu vermeiden

makeAllTitles <- function(activeItem){
  
  ## Cosine-KNN fÃ¼r TM #########################################################
  
  activeAuthor1 <- activeAuthorAndTitel$author[activeItem] # hier loopen
  activeAuthor1
  
  activeTitel1 <- activeAuthorAndTitel$title[activeItem] # hier loopen
  activeTitel1
  
  aA1_NeighorsCos <- CosineSparse[activeAuthor1,][CosineSparse[activeAuthor1,] != 0]
  
  if(length(aA1_NeighorsCos[aA1_NeighorsCos == 1]) < 100){
    
    aA1_top10NeigbhorsCos <- sort(aA1_NeighorsCos, decreasing = TRUE)[1:100]
    
    aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
    
    # Extrahiere alle weiteren Buchtitel des Ziel-Autoren 
    allTitelActAuth1 <- totalInfo %>% 
      filter(author == activeAuthor1 & title != activeTitel1)
    
    # Extrahieren aller Buchtitel der kNN
    allTitelKNN <- totalInfo %>% 
      filter(author %in% aA1_top10NeigbhorsCos)
    
    # Jetzt mÃ¼ssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengefÃ¼hrt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- totalInfo %>% 
      filter(title == activeTitel1) 
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeiÃen
    allTitles <- allTitles %>% distinct(itemID, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    return(allTitles)
    
  } else {
    
    aA1_top10NeigbhorsCos <-  aA1_NeighorsCos[aA1_NeighorsCos == 1] 
    # fraglich, ob man hier nicht vielleicht ">= 0.9" nimmt, anstatt "== 1"
    
    aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
    
    # Bevor man diese diese nun fÃ¼r Text Mining verwenden kann, muss man eine doc_id
    # in Spalte 1 hinzufgen und die Spalte 2 in 'text' umbenennen
    
    # Extrahiere alle weiteren Buchtitel des Ziel-Autoren 
    allTitelActAuth1 <- totalInfo %>% 
      filter(author == activeAuthor1 & title != activeTitel1) 
    
    # Extrahieren aller Buchtitel der kNN
    allTitelKNN <- totalInfo %>% 
      filter(author %in% aA1_top10NeigbhorsCos) 
    
    # length(unique(allTitelKNN$text)) nochmal in Ruhe anschauen
    
    if(nrow(allTitelKNN) > 300){
      allTitelKNN <- totalInfo %>% 
        filter(author %in% aA1_top10NeigbhorsCos) %>% 
        filter(clicked == TRUE | basket == TRUE | ordered == TRUE)
    } 
    
    if(nrow(allTitelKNN) > 300){
      allTitelKNN <- totalInfo %>% 
        filter(author %in% aA1_top10NeigbhorsCos) %>% 
        filter(basket == TRUE | ordered == TRUE) 
    } 
    
    if(nrow(allTitelKNN) > 300){
      allTitelKNN <- totalInfo %>% 
        filter(author %in% aA1_top10NeigbhorsCos) %>% 
        filter(ordered == TRUE) 
    } 
    
    # Jetzt mÃ¼ssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengefÃ¼hrt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- totalInfo %>% 
      filter(title == activeTitel1)
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeiÃen
    allTitles <- allTitles %>% distinct(itemID, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    return(allTitles)
    
  }
  
}


### Definition von TM_Recommendations

# Nachfolgend soll eien Funktion definiert werden, welche basierend auf
# Titel-Und-Klappentext-Similarity einen Vektor mit itemID's ausgibt,
# um so Buch/Item-Empfehlungen aussprechen zu kÃ¶nnen. Diese werden dann im
# weiteren Verlauf den anderen Komponenten dieses hybriden Systems gegenÃ¼ber
# gestellt.


TM_Recommendations <- function(activeItem){
  
  allTitles <- makeAllTitles(activeItem)
  
  allTitles <- allTitles %>% 
    select(itemID, titleUndBeschreibung) %>% 
    rename(doc_id = itemID, text = titleUndBeschreibung)
  
  DTM <- makeDTM(allTitles)
  
  # Da man eine DTM nicht in eine sparseMatrix transformieren kann - das jedoch
  # ein zentrales Kriterium fÃ¼r proxyC ist - mÃ¼ssen wir die cosine-Function aus
  # einem anderen Paket benutzen
  
  tic("Dauer Cosine Similarity der Titel")
  parallelStartSocket(cpus = detectCores())
  
  CosineTitel <- lsa::cosine(t(as.matrix(DTM)))
  
  parallelStop()
  toc()
  
  # In einem letzten Schritt lassen wir uns nun noch die 5 nÃ¤chsten Nachbarn,
  # basierend auf der Ãhnlichkeit der Buchtitel ausgeben
  
  top5Recc <- sort(CosineTitel[as.character(allTitles[1, ]$doc_id),], decreasing = TRUE)
  top5Recc <- top5Recc[names(top5Recc) != as.character(allTitles[1, ]$doc_id)]#[1:5]
  topRecc <- top5Recc[top5Recc != 0]
  # hier kann man Ã¼berlegen, ob != 0 Sinn macht, oder man nicht stattdessen sowas nimmt
  # wie >= 0.05 um Zufallstreffer rauszufiltern
  
  #print(paste("For Item", activeTitel1, "the top Recommendations are:", sep = " "))
  return(topRecc)
  
}

### Definition von UniteTops_Recommendations

UniteTopics_Recommendations <- function(activeItem) {
  
  allTitles <- makeAllTitles(activeItem)
  
  itemIDs <- as.character(allTitles$itemID)
  
  itemMatKNN <- itemMatSparse[itemIDs,]
  
  tic("Dauer Cosine mit proxyC und drop0 = TRUE")
  parallelStartSocket(cpus = detectCores())
  
  kNNCosine <- proxyC::simil(itemMatKNN, method = "cosine", drop0 = TRUE)
  
  parallelStop()
  toc()
  
  # In einem letzten Schritt lassen wir uns nun noch die 5 nÃ¤chsten Nachbarn,
  # basierend auf der Ãhnlichkeit der Buchtitel ausgeben
  
  top5Recc <- sort(kNNCosine[as.character(allTitles[1, ]$itemID),], decreasing = TRUE)
  top5Recc <- top5Recc[names(top5Recc) != as.character(allTitles[1, ]$itemID)]#[1:5]
  topRecc <- top5Recc[top5Recc != 0]
  # hier kann man Ã¼berlegen, ob != 0 Sinn macht, oder man nicht stattdessen sowas nimmt
  # wie >= 0.05 um Zufallstreffer rauszufiltern
  
  #print(paste("For Item", activeTitel1, "the top Recommendations are:", sep = " "))
  return(topRecc)
  
}

### Test der Funktionen

tic("Start TM_Rec")  
TM_Recommendations(999)
toc()

tic("Start ST_Rec")
UniteTopics_Recommendations(999)
toc()