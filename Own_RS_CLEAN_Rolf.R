# Neuer Ansatz: Eigenes Recommender-System basierend auf den Vorschlägen vom 16.05.2021

# Autoren-Similarity basierend auf den Main-Topics berechnen.
# Ziel: Eine Matrix mit den Autoren in den Zeilen und den Main-Topics in den Spalten
# In den Zellen stehen dann die Anzahl der Bücher des jeweiligen Autoren im jeweiligen Genre
# Damit die Matrix nicht ausufert müssen wir vorher die Main-Topics "zusammenstampfen".
# Grund: Somit können wir die Autoren gruppieren und bspw. die Roman-Autoren von Kochbuch-Autoren trennen.
# Somit schränken wir auch sofort die Büchersuche auf diese Bereiche ein und sparen uns so Rechenzeit,
# die verloren gehen würde, wenn wir in "sinnbefreiten" Genre suchen würden.

# Working Directory Definieren

getwd()
setwd("C:/Users/Rolf/Desktop/WFI - Unterlagen/Master/4. Semester/Data Analytics Challenge/Data_Analytics_Challenge")

# Packages laden

library(tidyverse)
library(tictoc)
library(parallelMap)
library(parallel)
library(proxyC)
library(tm)

# Datensatz laden

# Da wir ja bereits festegestellt haben, dass diverse Autoren häufiger unter einem anderen Namen vorkommen,
# verwenden wir die von Eric mittels Open Refine vorbehandelte Matrix. In dieser sind bestenfalls auch schon
# die Main Topics auf die ersten zwei, drei Buchstaben zusammengestampft.

## Datenaufbereitung ############################################################

#items3 <- read.csv("./Data/items3.csv")

items5 <- read.csv("./Data/items5.csv", encoding = "UTF-8")

#items6 <- read.csv("./Data/items6.csv", encoding = "UTF-8")

# Matrix aufbauen, mit den Dimensionen entsprechend der Anzahl der Autoren und Genre
AuthMat <- items5 %>% 
  group_by(author, main.topic) %>% 
  dplyr::summarise(n = n()) %>% 
  spread(key = main.topic, value = n)

# Autorennamen abspeichern, da diese später als rownames eingefügt werden
Authors <- AuthMat$author

apply(AuthMat,2,class)

# komischerweise werden die Spalten als character dargestellt, deswegen müssen die Variablen
# in numerische Werte transformiert werden, damit die Korrelationen zwischen den Autoren berechnet
# werden können

AuthMat <- apply(AuthMat[2:ncol(AuthMat)],2,as.integer)

# Überprüfung, ob Transformation funktioniert hat
apply(AuthMat,2,class)
head(AuthMat)

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

AuthMat0 <- AuthMat
AuthMat0[is.na(AuthMat0)] <- 0
mode(AuthMat0) <- "integer"
# dim(AuthMat0)
# length(Authors)
rownames(AuthMat) <- Authors
rownames(AuthMat0) <- Authors

AuthMat0sparse <- as(AuthMat0, "sparseMatrix")

## Anwendung von proxyC -#######################################################

rm(Authors)
rm(AuthMat)

#AuthMat0sparse <- as(AuthMat0, "sparseMatrix")
rm(AuthMat0)

## Anwendung Cosine - akutelle Alternative, da schneller #######################

tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelStartSocket(cpus = detectCores())

CosineSparse <- proxyC::simil(AuthMat0sparse, method = "cosine", drop0 = TRUE)

parallelStop()
toc()

#Dauer: 25.83 Sekunden
# Beim zweiten Mal hat es geringfügig länger gedauert, nämlich 28.63 Sekunden
object.size(CosineSparse)
# Größe: 648259360 byte = ca. 0.68 GB
# Und auch beim zweiten Mal ist das Objekt exakt genauso groß

### ENDE Phase 1 ###############################################################

### BEGINN Phase 2 #############################################################

# Somit ist Phase 1 - die Ermittlung der Ähnlichkeite der Autoren basierend auf 
# den Main Topics - abgeschlossen.
# Im nächsten Schritt müssen wir uns die Zielautoren ausgeben lassen,
# anschließend können wir in der Matrix nach besagtem Autor suchen und uns
# dessen kNearestNeighbors sowie deren geschriebene Bücher ausgeben lassen.

# Import von Items
items5 <- read.csv("./Data/items5.csv", encoding = "UTF-8")

items5[duplicated(items5$title),] %>% nrow()
# haben 6474 Titel die gleich hei?en - m?ssen wir rausschmei?en

# Die Subtopics, die nur [] sind, sollen durch NAs ersetzt werden
#subtopics <- ifelse(items5$subtopics == "[]", NA, items5$subtopics)
#items5$subtopics <- subtopics
#sum(is.na(subtopics))
# 33643 Items haben keine Subtopics

# Remove duplicates
#items5 <- items5 %>% distinct(title, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
#items5titleSubTop <- items5 %>% distinct(title, subtopics, .keep_all = TRUE) # schmei?t nur die mit gleichem titel und gleiche Subtopics raus

# DARF MAN SO NICHT MACHEN!!! Sonst fliegen auch ItemIDs raus, die in Evaluation sind. Man kann dann nicht mehr joinen -.-'

## Import der Transactions-Daten
transactions <- as.tibble(read.csv("./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F))

# Verknüpfen von Transactions mit Items, sodass eine weitere Spalte angefügt wird,
# die ausgibt, ob ein Item in dup.ses vorkommt
joined_oR <- left_join(transactions, items5, by = "itemID")

dup.ses <- joined_oR[joined_oR$sessionID %in%
                       joined_oR$sessionID[duplicated(joined_oR$sessionID)],]

itemsInSes <- dup.ses %>% select(itemID) %>% unique() %>% mutate(inDupSes = TRUE)
itemsWithDupSes <- items5 %>% left_join(itemsInSes, by = "itemID")
itemsWithDupSes$inDupSes[is.na(itemsWithDupSes$inDupSes)] <- FALSE

# Import der Crawler-Daten
FCD <- readRDS("./Data/FinaleCrawlerDatenUpdated.rds")

# https://stackoverflow.com/questions/49564748/extract-multiple-elements-from-a-list-of-lists-lapply
FCD_tibble <- as_tibble(do.call("rbind", lapply(FCD, '[', c(1, 15))))

sum(is.na(FCD_tibble$Beschreibung))
# 13732 Items haben keine Klappentexte

FCD_tibble <- FCD_tibble %>% 
  rename(title = Titel) %>% 
  mutate(title = unlist(title)) %>% 
  mutate(Beschreibung = unlist(Beschreibung)) #%>% 
#mutate(title = toupper(title)) %>% 
#mutate(Beschreibung = toupper(Beschreibung))

# Auch hier m?ssen wir noch die duplicates raushauen
FCD_tibble <- FCD_tibble %>% distinct(title, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus

# Hinzuf?gen der Beschreibung zu items

totalInfo <- itemsWithDupSes %>% 
  left_join(FCD_tibble, by = "title")

totalInfo$subtopics <- gsub("\\[|\\]", "", totalInfo$subtopics)
totalInfo$subtopics <- gsub(","," ", totalInfo$subtopics)

totalInfo <- totalInfo %>% 
  mutate(MainAndSub = paste(main.topic, subtopics, sep = " ")) # ACHTUNG: Hier kanne es noch zu überschneidungen kommen!

#SubTopAndBeschr <- itemsUndBeschreibung %>% 
#  filter(!is.na(subtopics) & !is.na(Beschreibung) )
# hier erscheinen pl?tzlich viel mehr Items
# das liegt daran, dass manche B?cher mehrmals vorkommen, allerdings teilweise
# andere Klappentexte (oder Erscheinungsformen) haben

# Neue Spalte erstellen, welche Title und Klappentext verkn?pft.
# So kann man immerhin ?ber die Titel die Similarity bestimmen, wenn kein
# Klappentext verf?gbar ist

itemsUndBeschreibung <- totalInfo %>% 
  mutate(titleUndBeschreibung = paste(title, Beschreibung, sep = " ")) %>% 
  select(itemID, author, inDupSes, title, titleUndBeschreibung)

#rm(items5)

# Laden des Evaluationsdatensatz
evaluation <- read.csv("./Data/evaluation.csv")

activeAuthorAndTitel <- evaluation %>% 
  left_join(itemsUndBeschreibung, by = "itemID") %>% 
  select(itemID, author, inDupSes, title, titleUndBeschreibung)

# test <- evaluation %>% 
#   left_join(items5, by = "itemID") %>% 
#   select(itemID, author, title)
# 
# SimIsOne <- rep(NA,nrow(test))  
# for(idx in 1:nrow(test)){
#   activeAuthor1 <- test$author[idx] # hier loopen
#   aA1_NeighorsCos <- CosineSparse[activeAuthor1,]
#   #aA1_top10NeigbhorsCos <- sort(aA1_NeighorsCos, decreasing = TRUE)[1:10] # Namen der Autoren und deren Similarities
#   #aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
#   SimIsOne[idx] <- length(aA1_NeighorsCos[aA1_NeighorsCos == 1])
# }  
# 
# # Hat ca. 3h gedauert -.-'
# 
# SimIsOne 
# 
# names(SimIsOne) <- test$author
# SimIsOne
# 
# hist(SimIsOne)
# 
# aA1 <- activeAuthorAndTitel$author[293]  
# aA1_N <- CosineSparse[aA1,]  
# length(aA1_N[aA1_N == 1]) 

## EINBETTUNG IN FUNKTION ######################################################

TM_Recommendations <- function(activeItem){
  
  ## Cosine-KNN für TM #########################################################
  
  activeAuthor1 <- activeAuthorAndTitel$author[activeItem] # hier loopen
  aA1_NeighorsCos <- CosineSparse[activeAuthor1,][CosineSparse[activeAuthor1,] != 0]
  
  if(length(aA1_NeighorsCos[aA1_NeighorsCos == 1]) < 100){
    
    aA1_top10NeigbhorsCos <- sort(aA1_NeighorsCos, decreasing = TRUE)[1:10]
    
    aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
    #length(aA1_NeighorsCos[aA1_NeighorsCos == 1])
    #hist(aA1_NeighorsCos, main = "aA1_NeighorsCos")
    #summary(aA1_NeighorsCos)
    # hier ist es das gleiche Problem, es gibt zu viele Autoren mit einer Similarity
    # von 1 - genauer gesagt 3674.
    
    # Test: Autoren mit Similarity != 1
    #aA1_top10NeigbhorsCos <- sort(aA1_NeighorsCos[aA1_NeighorsCos != 1], decreasing = TRUE)[1:10]
    #aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos)
    # Ist eigentlich nicht mal so schlecht!
    
    # Die Anzahlen von Autoren, welche eine Similarity von 1 aufweisen, ist identisch
    # zwischen Jaccard und Cosine. Gibt es dann überhaupt Unterschiede oder ist auch
    # der Rest der Vektoren identisch?
    # identical(aA1_NeighorsJacc, aA1_NeighorsCos)
    # all.equal(aA1_NeighorsJacc, aA1_NeighorsCos)
    # Das ist offenbar nicht der Fall, es liegen Unterschiede vor
    
    ## Text Mining #################################################################
    
    # Angenommen wir hätten jetzt die 10 ähnlichsten Autoren + den Autoren selbst, 
    # dann müssen im Folgenden all ihre Bücher bzw. deren Titel extrahieren.
    # Im Anschluss daran betten wir den Titel des aktiven Buchs mit all denn anderen
    # Buchtiteln in eine TF-IDF-Matrix ein und berechnen dann für dieses Buch
    # die 5 nächsten Nachbarn, basierend auf der Cosine Similarity.
    
    # Zunächst: Extrahieren des Zielbuchtitels und des Klappentextes:
    activeTitel1 <- activeAuthorAndTitel$title[activeItem] # hier loopen
    activeTitel1
    
    # Bevor man diese diese nun für Text Mining verwenden kann, muss man eine doc_id
    # in Spalte 1 hinzufgen und die Spalte 2 in 'text' umbenennen
    
    # Extrahieren aller Buchtitel des Ziel-Autors, außer des Ziel-Titels
    allTitelActAuth1 <- itemsUndBeschreibung %>% 
      filter(author == activeAuthor1 & title != activeTitel1) %>% 
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    # Extrahieren aller Buchtitel der kNN
    allTitelKNN <- itemsUndBeschreibung %>% 
      filter(str_detect(.$author, paste(aA1_top10NeigbhorsCos, collapse = "|"))) %>%
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    # Jetzt müssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengeführt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- itemsUndBeschreibung %>% 
      filter(title == activeTitel1) %>% 
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeißen
    allTitles <- allTitles %>% distinct(doc_id, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    #allTitles
    
    Corpus <- VCorpus(DataframeSource(allTitles))
    #Corpus
    
    # Empfohlenes Pre-Processing aus 'Modern Data Science with R'
    Corpus <- Corpus %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("english")) %>% 
      tm_map(removeWords, stopwords("german"))
    # Kann nicht einfach noch deutsche Stopwords entfernen, da dann englische Wörter wie 'Die'
    # ebenfalls gelöscht werden würden :-/
    
    #strwrap(as.character(Corpus[[10]]))
    # Eigentlich müsste man dan den Items-Datensatz in Sub-Datensätze nach Sprache
    # aufteilen, um effektiv Stopwords entfernen zu können.
    # Oder man pickt aus den 3500 Neighbors nur die mit der gleichen Sprache raus
    
    ## Visualisierung als Wourdcloud
    
    # library(wordcloud)
    # wordcloud(Corpus, max.words = 30, scale = c(8, 1),
    #           colors = topo.colors(n = 30), random.color = TRUE)
    # War nicht wirklich aussagekräftig ;-)
    
    # Als nächstes wird der Corpus in eine TF-IDF-Matrix überführt:
    DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
    #inspect(DTM)
    
    # Da man eine DTM nicht in eine sparseMatrix transformieren kann - das jedoch
    # ein zentrales Kriterium für proxyC ist - müssen wir die cosine-Function aus
    # einem anderen Paket benutzen
    
    tic("Dauer Cosine Similarity der Titel")
    parallelStartSocket(cpus = detectCores())
    
    CosineTitel <- lsa::cosine(t(as.matrix(DTM)))
    
    parallelStop()
    toc()
    
    # Ginge auch mit dem "herkömmlichen" proxy-Package, allerdings wird dort nur ein
    # Dreieck erzeugt, während bei lsa sofort die ganze Matrix samt Hauptdiagonale
    # erzeugt wird: proxy::simil(as.matrix(DTM), method = "cosine", upper = TRUE)
    
    # In einem letzten Schritt lassen wir uns nun noch die 5 nächsten Nachbarn,
    # basierend auf der Ähnlichkeit der Buchtitel ausgeben
    
    top5Recc <- sort(CosineTitel[as.character(activeTitel1MitBeschreibung$doc_id),], decreasing = TRUE)
    #top5Recc <- sort(CosineTitel["45274",], decreasing = TRUE)
    top5Recc <- top5Recc[names(top5Recc) != as.character(activeTitel1MitBeschreibung$doc_id)]#[1:5]
    #top5Recc <- top5Recc[names(top5Recc) != "45274"][1:5]
    
    print(paste("For Item", activeTitel1, "the top 5 Recommendations are:", sep = " "))
    return(top5Recc)
    
  } else {
    
    DupSesactiveAuthorAndTitel <- activeAuthorAndTitel %>% filter(inDupSes == TRUE)
    DupSesitemsUndBeschreibung <- itemsUndBeschreibung %>% filter(inDupSes == TRUE)
    
    # Angenommen wir hätten jetzt die 10 ähnlichsten Autoren + den Autoren selbst, 
    # dann müssen im Folgenden all ihre Bücher bzw. deren Titel extrahieren.
    # Im Anschluss daran betten wir den Titel des aktiven Buchs mit all denn anderen
    # Buchtiteln in eine TF-IDF-Matrix ein und berechnen dann für dieses Buch
    # die 5 nächsten Nachbarn, basierend auf der Cosine Similarity.
    
    # Zunächst: Extrahieren des Zielbuchtitels und des Klappentextes:
    activeTitel1 <- activeAuthorAndTitel$title[activeItem] # hier loopen
    activeTitel1
    
    # Bevor man diese diese nun für Text Mining verwenden kann, muss man eine doc_id
    # in Spalte 1 hinzufgen und die Spalte 2 in 'text' umbenennen
    
    # Extrahieren aller Buchtitel des Ziel-Autors, außer des Ziel-Titels
    allTitelActAuth1 <- itemsUndBeschreibung %>% 
      filter(author == activeAuthor1 & title != activeTitel1) %>% 
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    # Extrahieren aller Buchtitel der kNN
    allTitelKNN <- DupSesitemsUndBeschreibung %>% 
      filter(str_detect(.$author, paste(aA1_top10NeigbhorsCos, collapse = "|"))) %>%
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    # Jetzt müssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengeführt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- itemsUndBeschreibung %>% 
      filter(title == activeTitel1) %>% 
      select(itemID, titleUndBeschreibung) %>% 
      rename(doc_id = itemID, text = titleUndBeschreibung)
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeißen
    allTitles <- allTitles %>% distinct(doc_id, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    #allTitles
    
    Corpus <- VCorpus(DataframeSource(allTitles))
    #Corpus
    
    # Empfohlenes Pre-Processing aus 'Modern Data Science with R'
    Corpus <- Corpus %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("english")) %>% 
      tm_map(removeWords, stopwords("german"))
    # Kann nicht einfach noch deutsche Stopwords entfernen, da dann englische Wörter wie 'Die'
    # ebenfalls gelöscht werden würden :-/
    
    strwrap(as.character(Corpus[[10]]))
    # Eigentlich müsste man dan den Items-Datensatz in Sub-Datensätze nach Sprache
    # aufteilen, um effektiv Stopwords entfernen zu können.
    # Oder man pickt aus den 3500 Neighbors nur die mit der gleichen Sprache raus
    
    ## Visualisierung als Wourdcloud
    
    # library(wordcloud)
    # wordcloud(Corpus, max.words = 30, scale = c(8, 1),
    #           colors = topo.colors(n = 30), random.color = TRUE)
    # War nicht wirklich aussagekräftig ;-)
    
    # Als nächstes wird der Corpus in eine TF-IDF-Matrix überführt:
    DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
    #inspect(DTM)
    
    # Da man eine DTM nicht in eine sparseMatrix transformieren kann - das jedoch
    # ein zentrales Kriterium für proxyC ist - müssen wir die cosine-Function aus
    # einem anderen Paket benutzen
    
    tic("Dauer Cosine Similarity der Titel")
    parallelStartSocket(cpus = detectCores())
    
    CosineTitel <- lsa::cosine(t(as.matrix(DTM)))
    
    parallelStop()
    toc()
    
    # Ginge auch mit dem "herkömmlichen" proxy-Package, allerdings wird dort nur ein
    # Dreieck erzeugt, während bei lsa sofort die ganze Matrix samt Hauptdiagonale
    # erzeugt wird: proxy::simil(as.matrix(DTM), method = "cosine", upper = TRUE)
    
    # In einem letzten Schritt lassen wir uns nun noch die 5 nächsten Nachbarn,
    # basierend auf der Ähnlichkeit der Buchtitel ausgeben
    
    top5Recc <- sort(CosineTitel[as.character(activeTitel1MitBeschreibung$doc_id),], decreasing = TRUE)
    #top5Recc <- sort(CosineTitel["45274",], decreasing = TRUE)
    top5Recc <- top5Recc[names(top5Recc) != as.character(activeTitel1MitBeschreibung$doc_id)]# [1:5]
    #top5Recc <- top5Recc[names(top5Recc) != "45274"][1:5]
    
    print(paste("For Item", activeTitel1, "the top 5 Recommendations are:", sep = " "))
    return(top5Recc)
    
  }
  
}

tic("Start function")  
TM_Recommendations(2)
toc()
