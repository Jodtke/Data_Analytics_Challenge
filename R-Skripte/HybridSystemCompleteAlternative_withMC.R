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

### DatensÃÂÃÂ¤tze laden

# Datensatz mit Item-Informationen
items6 <- read.csv("./Data/items6.csv", encoding = "UTF-8")

# Datensatz mit Transaktionsdaten
transactions <- as_tibble(read.csv("./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F))

# Crawler-Daten mit Klappentexten
FCD_tibble <- as_tibble(read.csv("./Data/KlappentexteUndTitel.csv", encoding = "UTF-8"))

# Evaluations-Daten
evaluation <- read.csv("./Data/evaluation.csv")

### DatensÃÂÃÂ¤tze transformieren und ergÃÂÃÂ¤nzende erstellen

### Cosine Similarity Matrix erstellen (basierend auf Subtopics)

# Erstelle Similarity-Matrix, welche die Cosine Similarity und somit die
# ÃÂÃÂhnlichkeit zwischen den Autoren berechnet.
# Dabei wird BerÃÂÃÂ¼cksichtigt, wie viele BÃÂÃÂ¼cher die Autoren im jeweiligen
# Main Topic und Sub Topic geschrieben haben

# Imporiteren der AuthMat_subtopics aus anderem Skript
AuthMat <- read.csv("./Data/AuthMat_subtopics.csv", check.names = FALSE)

# Autorennamen abspeichern, da diese spÃÂÃÂ¤ter als rownames eingefÃÂÃÂ¼gt werden
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

# Nicht mehr benÃÂ¶tigte DatensÃÂ¤tze lÃÂ¶schen, um Speicherplatz zu sparen
rm(Authors, AuthMat, AuthMatSparse)

### Erstellung Item-Matrix, welche AusprÃÂ¤gung in entsprechendem Subtopic enthÃÂ¤lt

# Imporiteren der itemMat_subtopics aus anderem Skript
itemMat <- read.csv("./Data/itemMat_subtopics.csv", check.names = FALSE)

# ItemID's abspeichern, da diese spÃÂ¤ter als rownames eingefÃÂ¼gt werden
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

# Nicht mehr benÃÂ¶tigte DatensÃÂ¤tze lÃÂ¶schen, um Speicherplatz zu sparen
rm(itemIDs, itemMat)

### joined_oR erstellen

# ZusammenfÃÂÃÂ¼gen der Transaktions- und Item-Daten, welche als Ausgangspunkt
# zur Gewinnung weiterer Features dienen
joined_oR <- left_join(transactions, items6, by = "itemID")

# Erstellung von dup.ses, um im spÃÂÃÂ¤teren Verlauf eine Art ARM anwenden zu kÃÂÃÂ¶nnen
dup.ses <- joined_oR[joined_oR$sessionID %in%
                       joined_oR$sessionID[duplicated(joined_oR$sessionID)],]

# Erstellung der Variable inDupSes, welche darÃÂÃÂ¼ber informiert, ob sich ein Item
# in Dup.Ses befindet oder nicht
inDupSes <- dup.ses %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(inDupSes = TRUE)

# Erstellung der Variable inTransactions, welche darÃÂÃÂ¼ber informiert, ob sich ein 
# Item transactions befindet oder nicht 
itemsInTrans <- joined_oR %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(inTransactions = TRUE)

# Erstellung der Variable clicked, welche darÃÂÃÂ¼ber informiert, ob ein Item 
# mindestens einmal angeklickt wurde oder nicht 
clicked <- joined_oR %>% 
  filter(click > 0) %>%
  select(itemID) %>% 
  unique() %>% 
  mutate(clicked = TRUE)

# Erstellung der Variable basket, welche darÃÂÃÂ¼ber informiert, ob ein Item 
# mindestens einmal in den Warenkorb gelegt wurde oder nicht
basket <- joined_oR %>%
  filter(basket > 0) %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(basket = TRUE)

# Erstellung der Variable ordered, welche darÃÂÃÂ¼ber informiert, ob ein Item 
# mindestens einmal gekauft wurde oder nicht
ordered <- joined_oR %>%
  filter(order > 0) %>% 
  select(itemID) %>% 
  unique() %>% 
  mutate(ordered = TRUE)

### Reduzierung der Crawler-Daten

# Da diverse BÃÂÃÂ¼cher mit dem identischen Titel vorliegen, die Item-Daten mit den
# Crawler-Daten jedoch ÃÂÃÂ¼ber die Titel gejoint werden, mÃÂÃÂ¼ssen zunÃÂÃÂ¤chst alle
# Duplikate basierend auf den Titeln aussoritert werden.
FCD_tibble <- FCD_tibble %>% distinct(title, .keep_all = TRUE)
# ACHTUNG: Hier besteht immer noch Verbesserungspotential!
# Aktuell wird ja einfach nur das erste Item beibehalten, alle nachfolgenden
# werden aussortiert. Dabei kann es ja aber durchaus passieren, dass das erste
# Item keinen Klappentext hatte, die Duplikate jedoch schon. So gehen die
# Klappentexte verloren!

### Erstellung des tibbles totalInfo

# Erstelle tibble totalInfo, welches folgende Informationen enthÃÂÃÂ¤lt:
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

# VerknÃÂÃÂ¼pfung items6 mit itemsInTrans ÃÂÃÂ¼ber die itemID
totalInfo <- items6 %>% left_join(itemsInTrans, by = "itemID")
# ÃÂÃÂberschreibung der NA mit FALSE
totalInfo$inTransactions[is.na(totalInfo$inTransactions)] <- FALSE

# VerknÃÂÃÂ¼pfung von totalInfo mit clicked ÃÂÃÂ¼ber die itemID
totalInfo <- totalInfo %>% left_join(clicked, by = "itemID")
# ÃÂÃÂberschreibung der NA mit FALSE
totalInfo$clicked[is.na(totalInfo$clicked)] <- FALSE

# VerknÃÂÃÂ¼pfung von totalInfo mit basket ÃÂÃÂ¼ber die itemID
totalInfo <- totalInfo %>% left_join(basket, by = "itemID")
# ÃÂÃÂberschreibung der NA mit FALSE
totalInfo$basket[is.na(totalInfo$basket)] <- FALSE

# VerknÃÂÃÂ¼pfung von totalInfo mit ordered ÃÂÃÂ¼ber die itemID
totalInfo <- totalInfo %>% left_join(ordered, by = "itemID")
# ÃÂÃÂberschreibung der NA mit FALSE
totalInfo$ordered[is.na(totalInfo$ordered)] <- FALSE

# VerknÃÂÃÂ¼pfung von totalInfo mit inDupSes ÃÂÃÂ¼ber die itemID
totalInfo <- totalInfo %>% left_join(inDupSes, by = "itemID")
# ÃÂÃÂberschreibung der NA mit FALSE
totalInfo$inDupSes[is.na(totalInfo$inDupSes)] <- FALSE

# VerknÃÂÃÂ¼pfung von totalInfo mit der Beschreibung (Klappentext) ÃÂÃÂ¼ber den Titel
totalInfo <- totalInfo %>% left_join(FCD_tibble, by = "title")

# Erzeugung der Variable titelUndBeschreibung, um Text Mining zu verbessern
totalInfo <- totalInfo %>% mutate(titleUndBeschreibung = paste(title, Beschreibung, sep = " "))

# Nicht mehr benÃÂ¶tigte DatensÃÂ¤tze lÃÂ¶schen, um Speicherplatz zu sparen
rm(clicked, basket, ordered, inDupSes, itemsInTrans, items6, transactions, FCD_tibble)

### Erstellung von activeAuthorAndTitel

# VerknÃÂÃÂ¼pfung des Evaulation-Tibbles mit totalInfo um sÃÂÃÂ¤mtliche Informationen
# ÃÂÃÂ¼ber die Ziel-Items zu erhalten

activeAuthorAndTitel <- evaluation %>% left_join(totalInfo, by = "itemID")

# Nicht mehr benÃÂ¶tigte DatensÃÂ¤tze lÃÂ¶schen, um Speicherplatz zu sparen
rm(evaluation)

### Defintion von Funktionen

### Definition von normalize

# Kleine Helper-Funktion, welche uns hilft, die Recommendations aus den
# unterschiedlichen Komponenten des Hybriden auf einen Wertebereich zu bringen

normalize <- function(x){
  norm <- x / sum(x)
  return(norm)
}

### Definition der Funktion 'makeDTM'

# Da im weiteren Verlauf Fallunterscheidungen (if-Statements) auftreten, 
# mÃÂÃÂ¼sste der Code mehrmals kopiert und eingefÃÂÃÂ¼gt werden.Durch die Definition 
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
    tm_map(removeWords, c("NA","•","»","®", "–", "¡", "¿", "—"))
  
  # Als nÃÂÃÂ¤chstes wird der Corpus in eine TF-IDF-Matrix ÃÂÃÂ¼berfÃÂÃÂ¼hrt:
  DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
  
  return(DTM)
  
}

### Definition der Funktion 'makeAllTitles'

# Da sowohl fÃÂ¼r die Klappentext-Similarity als auch fÃÂ¼r die Subtopic-Similarity
# ÃÂ¤hnliche Vorselektionen vorgenommen werden, bietet es sich an, eine Funktion
# zu schreiben, die diese ÃÂ¼bernimmt, um Code-Redundanzen zu vermeiden

makeAllTitles <- function(activeItem){
  
  ## Cosine-KNN fÃÂÃÂ¼r TM #########################################################
  
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
    
    # Jetzt mÃÂÃÂ¼ssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengefÃÂÃÂ¼hrt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- totalInfo %>% 
      filter(title == activeTitel1) 
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeiÃÂÃÂen
    allTitles <- allTitles %>% distinct(itemID, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    return(allTitles)
    
  } else {
    
    aA1_top10NeigbhorsCos <-  aA1_NeighorsCos[aA1_NeighorsCos == 1] 
    # fraglich, ob man hier nicht vielleicht ">= 0.9" nimmt, anstatt "== 1"
    
    aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
    
    # Bevor man diese diese nun fÃÂÃÂ¼r Text Mining verwenden kann, muss man eine doc_id
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
    
    # Jetzt mÃÂÃÂ¼ssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
    # zusammengefÃÂÃÂ¼hrt und die doc_id entsprechend vergeben werden
    
    activeTitel1MitBeschreibung <- totalInfo %>% 
      filter(title == activeTitel1)
    
    allTitles <- rbind(activeTitel1MitBeschreibung, allTitelActAuth1, allTitelKNN)
    
    # Hier auch nochmal die duplicates rausschmeiÃÂÃÂen
    allTitles <- allTitles %>% distinct(itemID, .keep_all = TRUE) # schmei?t alle mit dem gleichen titel raus
    
    return(allTitles)
    
  }
  
}


### Definition von TM_Recommendations

# Nachfolgend soll eien Funktion definiert werden, welche basierend auf
# Titel-Und-Klappentext-Similarity einen Vektor mit itemID's ausgibt,
# um so Buch/Item-Empfehlungen aussprechen zu kÃÂÃÂ¶nnen. Diese werden dann im
# weiteren Verlauf den anderen Komponenten dieses hybriden Systems gegenÃÂÃÂ¼ber
# gestellt.

TM_Recommendations <- function(activeItem, simThresh = 0.014){
  
  allTitles <- makeAllTitles(activeItem)
  
  allTitles <- allTitles %>% 
    select(itemID, titleUndBeschreibung) %>% 
    rename(doc_id = itemID, text = titleUndBeschreibung)
  
  DTM <- makeDTM(allTitles)
  
  # Da man eine DTM nicht in eine sparseMatrix transformieren kann - das jedoch
  # ein zentrales Kriterium fÃÂÃÂ¼r proxyC ist - mÃÂÃÂ¼ssen wir die cosine-Function aus
  # einem anderen Paket benutzen
  
  tic("Dauer Cosine Similarity der Titel")
  parallelStartSocket(cpus = detectCores())
  
  CosineTitel <- lsa::cosine(t(as.matrix(DTM)))
  
  parallelStop()
  toc()
  
  # In einem letzten Schritt lassen wir uns nun noch die 5 nÃÂÃÂ¤chsten Nachbarn,
  # basierend auf der ÃÂÃÂhnlichkeit der Buchtitel ausgeben
  
  top5Recc <- sort(CosineTitel[as.character(allTitles[1, ]$doc_id),], decreasing = TRUE)
  top5Recc <- top5Recc[names(top5Recc) != as.character(allTitles[1, ]$doc_id)]#[1:5]
  # topRecc <- top5Recc[top5Recc != 0]
  # hier kann man ÃÂÃÂ¼berlegen, ob != 0 Sinn macht, oder man nicht stattdessen sowas nimmt
  # wie >= 0.05 um Zufallstreffer rauszufiltern
  topRecc <- top5Recc[top5Recc >= simThresh]
  topRecc <- normalize(topRecc)
  #print(paste("For Item", activeTitel1, "the top Recommendations are:", sep = " "))
  return(topRecc)
  
}

### Definition von UniteTops_Recommendations

UniteTopics_Recommendations <- function(activeItem, simThresh = 0.3) {
  
  allTitles <- makeAllTitles(activeItem)
  
  itemIDs <- as.character(allTitles$itemID)
  
  itemMatKNN <- itemMatSparse[itemIDs,]
  
  tic("Dauer Cosine mit proxyC und drop0 = TRUE")
  parallelStartSocket(cpus = detectCores())
  
  kNNCosine <- proxyC::simil(itemMatKNN, method = "cosine", drop0 = TRUE)
  
  parallelStop()
  toc()
  
  # In einem letzten Schritt lassen wir uns nun noch die 5 nÃÂÃÂ¤chsten Nachbarn,
  # basierend auf der ÃÂÃÂhnlichkeit der Buchtitel ausgeben
  
  top5Recc <- sort(kNNCosine[as.character(allTitles[1, ]$itemID),], decreasing = TRUE)
  top5Recc <- top5Recc[names(top5Recc) != as.character(allTitles[1, ]$itemID)]#[1:5]
  # topRecc <- top5Recc[top5Recc != 0]
  # hier kann man ÃÂÃÂ¼berlegen, ob != 0 Sinn macht, oder man nicht stattdessen sowas nimmt
  # wie >= 0.05 um Zufallstreffer rauszufiltern
  topRecc <- top5Recc[top5Recc >= simThresh] # 0.5 teilweise schon zu aggressiv
  topRecc <- normalize(topRecc)
  #print(paste("For Item", activeTitel1, "the top Recommendations are:", sep = " "))
  return(topRecc)
  
}

### Test der Funktionen

# tic("Start TM_Rec")  
# TM_Recommendations(999)
# toc()

# tic("Start ST_Rec")
# UniteTopics_Recommendations(999)
# toc()

################################################################################

#Recommendation Function based on sessionID

Recommendation_function_inDupSes <- function(activeItem,weightClicks, weightBasket, weightOrder){
  
  activeItemID <- activeAuthorAndTitel$itemID[activeItem] # hier loopen
  activeItemID
  
  this_ses <- dup.ses %>% group_by(sessionID) %>%   
    filter(itemID==activeItemID) %>% select(sessionID)
  
  this_ses <- this_ses$sessionID
  
  this_books_tbl <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID) 
  
  potential_recommendations <- this_books_tbl %>% group_by(itemID) %>% filter(itemID != activeItemID)%>% 
    summarise(nClick=sum(click),nBasket=sum(basket),nOrder=sum(order)) %>%
    arrange(desc(nClick))
  
  potential_recommendations <- potential_recommendations %>% 
    mutate(kennzahl = (nClick*weightClicks + nBasket*weightBasket + nOrder*weightOrder)/sum(weightClicks,weightBasket,weightOrder))
  
  potential_recommendations <- potential_recommendations %>% select(itemID,kennzahl) %>% arrange(desc(kennzahl)) %>%
    filter(itemID != activeItemID) #%>% slice_max(kennzahl,n=10)  
  
  item_names <- as.character(potential_recommendations$itemID)
  
  kpi <- potential_recommendations$kennzahl
  
  names(kpi) <- item_names
  
  kpi <- normalize(kpi)
  
  return(kpi)
}

### Test der Funktion
# Recommendation_function_inDupSes(17) # test fÃ¼r 'the hobbit'

##Recommendation Function schlechter Fall
Notausgang_funktion <- function(activeItem){
  
  activeItemID <- activeAuthorAndTitel$itemID[activeItem] # hier loopen
  activeItemID
  
  selected_features <- totalInfo %>% filter(itemID==activeItemID) %>%
    select(itemID,author,mainTopic,publisher)    #nehmen von OR_tbl ausgewÃÂ¤hlte Spalten
  
  this_author <- selected_features$author         #als chr darstellen 
  
  this_publisher <- selected_features$publisher
  
  this_genre <- selected_features$mainTopic
  
  items_select <- totalInfo %>% filter(author==this_author | publisher==this_publisher & mainTopic==this_genre)#filter einsetzen
  
  #set.seed(123)
  top_seller_by_mainTopic <- joined_oR %>%
    select(itemID, order, title, author, publisher, mainTopic) %>%
    filter(order >= 1) %>%
    group_by(mainTopic, itemID) %>%
    summarise(
      title = title,
      author = author,
      sumOrders = sum(order, na.rm=T),
      N = n(),
      .groups = "keep"
    ) %>%
    distinct(.keep_all = T)
  
  nimm_5 <- top_seller_by_mainTopic %>%
    filter(mainTopic == this_genre & itemID != activeItemID) %>%
    arrange(desc(sumOrders)) %>%
    head(n=5)
  
  nimm_5 <- nimm_5$itemID
  
  if (length(nimm_5) < 5) {
    
    values_left <- 5 - length(nimm_5)
    
    nimmRandom <- items_select %>% filter(itemID %in% items_select$itemID[!items_select$itemID %in% nimm_5])
    
    set.seed(123)
    nimmRandom <- slice_sample(nimmRandom, n = values_left)
    nimmRandom <- nimmRandom$itemID
    
    nimm_5 <- c(nimm_5, nimmRandom)
  }
  
  return(nimm_5)
  
}


### Test der Funktion
# Notausgang_funktion(999)

#Struktur von finalen Funktion
# activeItem <- 23 # 4 fÃ¼r Fall 1, 25 fÃ¼r Fall 2, 2 fÃ¼r Fall 3, 23 fÃ¼r Fall 4
# 
# weightTM = 0.33
# weightUT = 0.33
# weightDupSes = 0.34
# 
# weightClicks = 0.2
# weightBasket = 0.3
# weightOrder = 0.5
# 
# rm(activeItem, weightTM, weightUT, weightDupSes, weightClicks, weightBasekt, weightOrder)

HybridRecommendation <- function(activeItem, weightTM = 1/3, weightUT = 1/3, weightDupSes = 1/3, 
                                 weightClicks = 0.2, weightBasket = 0.3, weightOrder = 0.5){
  
  # Frage: soll ich dann, in den FÃ¤llen ab Fall 1 die Gewichtungen auf einen Wert von 1 skalieren,
  # sodass bspw. weightTM und weightUT als 0.5 dargestellt werden?
  # Hab ich jetzt mal gemacht - mehr oder weniger
  
  activeItemID <- activeAuthorAndTitel$itemID[activeItem]
  
  activeItemInfo <- totalInfo %>% filter(itemID==activeItemID)%>% select(itemID,uniteTopics,Beschreibung,inDupSes)
  
  hasSubtopics <- !activeItemInfo %>% select(uniteTopics) %>% is.na()
  
  hasKlappentext <- !activeItemInfo %>% select(Beschreibung) %>% is.na()
  
  #Fall_1: Subtopic + Klappentext + inDupSes = alle sind True
  if (hasSubtopics == TRUE & hasKlappentext == TRUE & activeItemInfo$inDupSes == TRUE){
    
    result_TM <- TM_Recommendations(activeItem) * weightTM
    itemsFromTM <- names(result_TM)
    
    result_UT <- UniteTopics_Recommendations(activeItem) * weightUT
    itemsFromUT <- names(result_UT)
    
    result_inDupSes <- Recommendation_function_inDupSes(activeItem, weightClicks, weightBasket, weightOrder) * weightDupSes
    itemsFromDupSes <- names(result_inDupSes)
    
    all_potential_recom <- c(itemsFromTM, itemsFromUT,itemsFromDupSes)
    
    finalTable <- sort(table(all_potential_recom),decreasing = T)
    
    equal3 <- which(finalTable == 3)
    
    RecomMatEqu3 <- cbind(result_TM[names(equal3)], result_UT[names(equal3)], result_inDupSes[names(equal3)])
    rownames(RecomMatEqu3) <- names(equal3) 
    
    finalRecomEqu3 <- RecomMatEqu3 %>% rowSums(na.rm = T) / sum(weightTM, weightUT, weightDupSes)
    #finalRecom <- finalRecom %>% sort(decreasing = T) %>% head(5)
    
    equal2 <- which(finalTable == 2)
    
    RecomMatEqu2 <- cbind(result_TM[names(equal2)], result_UT[names(equal2)], result_inDupSes[names(equal2)])
    rownames(RecomMatEqu2) <- names(equal2) 
    
    finalRecomEqu2 <- RecomMatEqu2 %>% rowSums(na.rm = T) / sum(weightTM, weightUT, weightDupSes)
    #finalRecom2 <- finalRecom2 %>% sort(decreasing = T) %>% head(values_left)
    
    equal1 <- which(finalTable == 1)
    
    RecomMatEqu1 <- cbind(result_TM[names(equal1)], result_UT[names(equal1)], result_inDupSes[names(equal1)])
    rownames(RecomMatEqu1) <- names(equal1) 
    
    finalRecomEqu1 <- RecomMatEqu1 %>% rowSums(na.rm = T) / sum(weightTM, weightUT, weightDupSes)
    #finalRecom3 <- finalRecom3 %>% sort(decreasing = T) %>% head(values_left)
    
    allRecom <- c(finalRecomEqu3, finalRecomEqu2, finalRecomEqu1)
    allRecom <- sort(allRecom, decreasing = TRUE)
    
    finalRecom <- head(allRecom, 5)
    
  }
  
  #Fall_2: Subtopic + inDupSes .   kein Klappentext == NA 
  if(hasSubtopics == TRUE & hasKlappentext == FALSE & activeItemInfo$inDupSes == TRUE){
    
    result_UT <- UniteTopics_Recommendations(activeItem) * weightUT
    itemsFromUT <- names(result_UT)
    
    result_inDupSes <- Recommendation_function_inDupSes(activeItem, weightClicks, weightBasket, weightOrder) * weightDupSes
    itemsFromDupSes <- names(result_inDupSes)
    
    all_potential_recom <- c(itemsFromUT,itemsFromDupSes)
    
    finalTable <- sort(table(all_potential_recom),decreasing = T)
    
    equal2 <- which(finalTable == 2)
    
    RecomMatEqu2 <- cbind(result_UT[names(equal2)],result_inDupSes[names(equal2)])
    rownames(RecomMatEqu2) <- names(equal2) 
    
    finalRecomEqu2 <- RecomMatEqu2 %>% rowSums(na.rm = T) / sum(weightUT, weightDupSes)
    #finalRecom <- finalRecom %>% sort(decreasing = T) %>% head(5)
    
    equal1 <- which(finalTable == 1)
    
    RecomMatEqu1 <- cbind(result_UT[names(equal1)], result_inDupSes[names(equal1)])
    rownames(RecomMatEqu1) <- names(equal1) 
    
    finalRecomEqu1 <- RecomMatEqu1 %>% rowSums(na.rm = T) / sum(weightUT, weightDupSes)
    #finalRecom2 <- finalRecom2 %>% sort(decreasing = T) %>% head(values_left)
    
    allRecom <- c(finalRecomEqu2, finalRecomEqu1)
    allRecom <- sort(allRecom, decreasing = TRUE)
    
    finalRecom <- head(allRecom, 5)
    
  }
  
  #Fall_3:Subtopic + Klappentext. Nicht in dup.ses.
  if (hasSubtopics == TRUE & hasKlappentext == TRUE & activeItemInfo$inDupSes == FALSE){
    
    result_TM <- TM_Recommendations(activeItem) * weightTM
    itemsFromTM <- names(result_TM)
    
    result_UT <- UniteTopics_Recommendations(activeItem) * weightUT
    itemsFromUT <- names(result_UT)
    
    all_potential_recom <- c(itemsFromTM, itemsFromUT)
    
    finalTable <- sort(table(all_potential_recom),decreasing = T)
    
    equal2 <- which(finalTable == 2)
    
    RecomMatEqu2 <- cbind(result_TM[names(equal2)], result_UT[names(equal2)])
    rownames(RecomMatEqu2) <- names(equal2) 
    
    finalRecomEqu2 <- RecomMatEqu2 %>% rowSums(na.rm = T) / sum(weightTM, weightUT)
    #finalRecom <- finalRecom %>% sort(decreasing = T) %>% head(5)
    
    equal1 <- which(finalTable == 1)
    
    RecomMatEqu1 <- cbind(result_TM[names(equal1)], result_UT[names(equal1)])
    rownames(RecomMatEqu1) <- names(equal1) 
    
    finalRecomEqu1 <- RecomMatEqu1 %>% rowSums(na.rm = T) / sum(weightTM, weightUT)
    #finalRecom2 <- finalRecom2 %>% sort(decreasing = T) %>% head(values_left)
    
    allRecom <- c(finalRecomEqu2, finalRecomEqu1)
    allRecom <- sort(allRecom, decreasing = TRUE)
    
    finalRecom <- head(allRecom, 5)
    
  }
  
  #Fall_4: Nur Subtopic Simularity, kein KT, nicht in Dup.ses.   #Obwohl ich bin mir nicht sicher, ob dieser Fall kommt oder soll gleich NOTAUSGANGFUNKTION angeschaltet werden
  if (hasSubtopics == TRUE & hasKlappentext == FALSE & activeItemInfo$inDupSes == FALSE){
    
    result_UT <- UniteTopics_Recommendations(activeItem) * weightUT
    itemsFromUT <- names(result_UT)
    
    all_potential_recom <- itemsFromUT
    
    finalTable <- sort(table(all_potential_recom),decreasing = T)
    
    equal1 <- which(finalTable == 1)
    
    RecomMatEqu1 <- cbind(result_UT[names(equal1)])
    rownames(RecomMatEqu1) <- names(equal1)
    
    finalRecomEqu1 <- RecomMatEqu1 %>% rowSums(na.rm = T) / sum(weightUT)
    
    allRecom <- finalRecomEqu1
    allRecom <- sort(allRecom, decreasing = TRUE)
    
    finalRecom <- head(allRecom, 5)
    
    if (length(finalRecom) < 5){
      
      values_left <- 5 - length(finalRecom)
      
      Notausgang <- Notausgang_funktion(activeItem)
      Notausgang <- as.character(Notausgang)[1:values_left]
      valuesNotausgang <- rep(0.000000, values_left)
      names(valuesNotausgang) <- Notausgang
      finalRecomNotausgang <- valuesNotausgang[1:values_left]
      
      finalRecom <- c(finalRecom, finalRecomNotausgang)
      
    }
    
  }
  
  print(paste("For Item", activeAuthorAndTitel$title[activeItem], "the top 5 Recommendations are:", sep = " "))
  return(finalRecom)
  
}

# # ### Test HybridRecommendation
# tic("Start Hybrid Recommendation")
# test <- HybridRecommendation(activeItem = 257) # weightTM = 0.1, weightUT = 0.2, weightDupSes = 0.7)
# toc()
# 
# test
# sum(test)
# FRAGE: Was passiert, wenn wir 5+ Items haben, mit einer Similarity von 1?
# Ist insbesondere im letzten Fall realistisch

# set.seed(123) 
# # set.seed(ix) wär eine Möglichkeit, um das nachverfolgbar zu machen, allerdings haben wir dann unterschiedliche W'keiten
# randomWeights <- sample(1:100, 3)
# randomWeights <- normalize(randomWeights)

# targetItem <- 257
# NumberOfRepetitions <- 3
# idx <- 3

MonteCarlo <- function(targetItem, NumberOfRepetitions){
  
  set.seed(1)
  randomWeightsBest <- sample(1:100, 3)
  randomWeightsBest <- normalize(randomWeightsBest)
  randomWeightsBest
  
  best <- HybridRecommendation(activeItem = targetItem, weightTM = randomWeightsBest[1], 
                               weightUT = randomWeightsBest[2], weightDupSes = randomWeightsBest[3])

  sumBest <- sum(best)
  sumBest
  
  for(idx in 2:NumberOfRepetitions){
    
    set.seed(idx)
    randomWeightsTest <- sample(1:100, 3)
    randomWeightsTest <- normalize(randomWeightsTest)
    randomWeightsTest
    
    test <- HybridRecommendation(activeItem = targetItem, weightTM = randomWeightsTest[1], 
                                 weightUT = randomWeightsTest[2], weightDupSes = randomWeightsTest[3])

    sumTest <- sum(test)
    sumTest
    
    if(sumTest > sumBest){
      
      randomWeightsBest <- randomWeightsTest
      sumBest <- sumTest
      
    }
    
  }
  
  SumAndWeights <- c(sumBest, randomWeightsBest)
  returnNames <- c("Sum of Similarities", "weightTM", "weightUT", "weightDupSes")
  names(SumAndWeights) <- returnNames
  
  return(SumAndWeights)
  
}

# tic("Testlauf Monte Carlo - 5 Kombinationen")
# Ergebnis <- MonteCarlo(257, 3)
# toc()
# Ergebnis

# multipleItems <- sample(1:100, 3)
# 
# NumberOfRepetitionsPerItem <- 3
# 
# jdx <- 1

MultipleMC <- function(multipleItems, NumberOfRepetitionsPerItem){
  
  resultMat <- matrix(NA, nrow = length(multipleItems), ncol = 5)
  resultMatNames <- c("ItemIndex", "Sum of Similarities", "weightTM", "weightUT", "weightDupSes")
  colnames(resultMat) <- resultMatNames
  resultMat
  
  for(jdx in 1:length(multipleItems)){
    
    itemIndex <- multipleItems[jdx]
    
    MC <- MonteCarlo(targetItem = itemIndex, NumberOfRepetitions = NumberOfRepetitionsPerItem)
    
    resultMatEntry <- c(itemIndex, MC)
    
    resultMat[jdx, ] <- resultMatEntry
    
  }
  
  return(resultMat)

}

x <- c(257, 999, 4)

tic("Testlauf Multiple Monte Carlo - 3 Items, 5 Iterationen pro Item")
ErgebnisMMC <- MultipleMC(x, 5)
toc()
ErgebnisMMC

colMeans(ErgebnisMMC[, 3:5])

# forItems <- 100
# 
# Recommendations <- matrix(NA, ncol = 5, nrow = forItems)
# 
# itemIDs <- activeAuthorAndTitel$itemID[1:forItems]
# 
# tic("Start Recommendations")
# for(idx in 1:forItems){
#   
#   ReccItems <- HybridRecommendation(activeItem = idx)
#   Recommendations[idx,] <- names(ReccItems)
#   
#   print(Recommendations[idx,])
#   print(paste("just finished item:", idx, sep = " "))
#   
# }
# rownames(Recommendations) <- as.character(itemIDs)
# Recommendations
# toc()

# write.csv(Recommendations, "First100Recommendations.csv")
# 
# top100 <- read.csv("First100Recommendations.csv")