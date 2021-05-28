# Neuer Ansatz: Eigenes Recommender-System basierend auf den Vorschlägen vom 16.05.2021

# Autoren-Similarity basierend auf den Main-Topics berechnen.
# Ziel: Eine Matrix mit den Autoren in den Zeilen und den Main-Topics in den Spalten
# In den Zellen stehen dann die Anzahl der Bücher des jeweiligen Autoren im jeweiligen Genre
# Damit die Matrix nicht ausufert müssen wir vorher die Main-Topics "zusammenstampfen".
# Grund: Somit können wir die Autoren gruppieren und bspw. die Roman-Autoren von Kochbuch-Autoren trennen.
# Somit schränken wir auch sofort die Büchersuche auf diese Bereiche ein und sparen uns so Rechenzeit,
# die verloren gehen würde, wenn wir in "sinnbefreiten" Genre suchen würden.

# Packages laden

library(tidyverse)

# Datensatz laden

# Da wir ja bereits festegestellt haben, dass diverse Autoren häufiger unter einem anderen Namen vorkommen,
# verwenden wir die von Eric mittels Open Refine vorbehandelte Matrix. In dieser sind bestenfalls auch schon
# die Main Topics auf die ersten zwei, drei Buchstaben zusammengestampft.

## Alt #########################################################################

# joined_item_trans <- read_csv(file = "./Data/joined_item_trans.csv", col_names = T, col_types = cols(
#   itemID=col_factor(),
#   title=col_character(),
#   author=col_character(),
#   publisher=col_character(),
#   main.topic=col_character(),
#   subtopics=col_character(),
#   sessionID=col_factor(),
#   click=col_integer(),
#   basket=col_integer(),
#   order=col_integer()
# ))
# head(joined_item_trans, n=10)
# glimpse(joined_item_trans)
# ## Reihenfolge der Spalten verändern
# joined_item_trans <- joined_item_trans[, c(7,1,8:10,2:6) ]
# head(joined_item_trans, n=10)

## Datenaufbereitung ############################################################

#items3 <- read.csv("./Data/items3.csv")

items5 <- read.csv("./Data/items5.csv")

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

## kMeans Clustering ###########################################################

# Zuvor wenden wir jedoch nch ein kMeans Clustering an, um einen Besseren
# Überblick über die Autoren und ihre Zusammenhänge zu erhalten:

# Ging mit AuthMat nicht, deswegen AuthMat0. Allerdings geht auch AuthMat0sparse
# Die Ergebnisse von AuthMat0 und AuthMat0sparse sind sogar identical!

library(tictoc)

tic("kMeans Clustering von AuthMat0sparse") # Ging mit AuthMat nicht, deswegen AuthMat0

set.seed(123)
k = 2
kMC_AuthMat0sparse <- kmeans(AuthMat0sparse, centers = k, iter.max = 20, algorithm = "MacQueen")  

toc()

# Tuning for k

set.seed(123)
kmeans_result <- kmeans(AuthMat0sparse, centers = 2, iter.max = 20, algorithm = "MacQueen")

within.ss <- rep(NA,30)

for (k in 1:30){
  kmeans_result <- kmeans(AuthMat0sparse, centers = k, iter.max = 50, algorithm = "MacQueen")
  within.ss[k] <- sum(kmeans_result$withinss)
  print(paste("Just finished k =", k))
}

#### Elbow criterion / method to visually find the optimal number of clusters
plot(within.ss)
lines(within.ss)

# Am besten wären 4 Cluster, also stellen wir k auf 4
kMeans_tuned <- kmeans(AuthMat0sparse, centers = 4, iter.max = 50, algorithm = "MacQueen")

# und überprüfen, ob die Cluster ungefähr gleich groß sind
sort(kMeans_tuned$cluster) %>% hist()
# das ist leider nicht der Fall. Offenbar müssen wir vorher noch normalisieren
# oder eien andere Form der Transformation (min-max) anwenden.
str(kMeans_tuned)


## Anwendung von proxy #########################################################

# tic("Dauer Jaccard Complete")
# AuthorJaccMat <- as.matrix(proxy::simil(AuthMat0, method = "cosine"))
# toc()

tic("Dauer Jaccard Triangle")
AuthorJaccTri0 <- proxy::simil(AuthMat0, method = "Jaccard")
toc()

# Matrix ist zu groß und kann mit dem proxy-Package nicht alloziert werden. 
# Eventuell könnte man dieses Problem beheben, wenn man die Matrix in eine 
# sparseMatrix konvertiert und das proxyC-package darauf anwendet.

# UPDATE 21.05.2021: Nachdem die Matrix unnötigerweise numeric-Werte enthielt,
# obwohl es sich lediglich um Integer gehandelt hat, habe ich die Werte in
# Integer umgewandelt und erneut proxy::simil darauf angwendet.
# Dies hat letztlich funktioniert und dabei 2790.99 Sekunden benötigt.

object.size(x = AuthorJaccTri0)

# also ca. 46,5 Minuten. Außerdem ist das Objekt 4986706160 bytes groß,
# also knapp 5 Gigabyte. Würde man dieses Objekt nun in eine Matrix überführen
# wollen, würde diese doppelt so groß sein, also knapp 10 GB benötigen.
# Dies ist mit unseren Rechnern letztlich nicht mehr händelbar.

## Tests mit proxyC ###########################################################

library(Matrix)

doc1 <- c(3,2,1,2,2,0,0)
doc2 <- c(2,1,0,1,2,0,0)
doc3 <- c(0,0,0,1,2,3,2)
doc4 <- c(0,0,1,0,1,2,3)
doc5 <- c(0,0,0,0,0,1,1) # hat keine Übereinstimmung mit doc1 
test <- rbind(doc1, doc2, doc3, doc4, doc5, doc1, doc2, doc3, doc4, doc5)

testTri <- proxy::simil(test, method = "jaccard")
object.size(testTri)
# 3264 bytes

sparseA <- as(test, "sparseMatrix")
# sparseB <- Matrix(test, sparse = TRUE) # Geht eher nicht so richtig

as.matrix(proxy::simil(test, method = "jaccard"))
as.matrix(proxyC::simil(sparseA, method = "jaccard"))

# hier kommen die gleichen Werte raus - bzw. wird mit proxyC auch noch die
# Diagonale mit 1ern ergänzt, was beim herkömmlichen proxy nicht der Fall ist.

class(proxyC::simil(sparseA, method = "jaccard"))
# Liefert ein Element der Klasse dsTMatrix

testProxyC0 <- proxyC::simil(sparseA, method = "jaccard", drop0 = TRUE)
object.size(testProxyC0) # 3392 bytes
testProxyC1 <- proxyC::simil(sparseA, method = "jaccard")
object.size(testProxyC1) # 3520 bytes
testProxyC2 <- proxyC::simil(sparseA, method = "jaccard", min_simil = 0.6)
object.size(testProxyC2) # 3072 bytes
testProxyC3 <- proxyC::simil(sparseA, method = "jaccard", rank = 3)
object.size(testProxyC3) # 3112 bytes

# Es wurden nun vier verschiedene Matrizen berechnet, wobei jede andere
# Parametereinstellungen aufweist. Die sparsamste Matrix war dabei ProxyC2
# welche eine min_simil vorgibt. Danach kommt ProxyC3 mit den ranks, gefolgt
# von ProxyC0 welche alle Nullen entfernt. Die größte Matrix war dabei die,
# welche keine Restriktionen hatte.

testProxyC4 <- proxyC::simil(sparseA, 
                             method = "jaccard", 
                             min_simil = 0.6,
                             rank = 3,
                             drop0 = TRUE)

object.size(testProxyC4)

# Wenn man es auf die Spitze treibt und mehrere Parameter kombiniert wird die
# Matrix sogar noch kleiner.

testProxyC1["doc1",]
# funktioniert, kann direkt darauf zugreifen
object.size(testProxyC3)
# 2552 bytes - ist zudem noch kleiner als das Dreieck von proxy!

# proxyC::simil(sparseA, method = "cosine")

## Anwendung von proxyC -#######################################################

rm(Authors)
rm(AuthMat)

AuthMat0sparse <- as(AuthMat0, "sparseMatrix")
rm(AuthMat0)

## Anwendung Jaccard - auskommentiert, weil zu lange ###########################

# # tic("Dauer Jaccard mit proxyC")
# # JaccardSparse <- proxyC::simil(AuthMat0sparse, method = "jaccard")
# # toc()
# # #Dauer: 7510 Sekunden = ca. 125 Minunten
# 
# # # # Die Bearbeitungszeit dauert leider sehr lange, zudem reicht der Arbeitsspeicher
# # # # immer noch nicht aus. Deshalb soll nun der virtuelle RAM vergrößert werden.
# # 
# # memory.size(max = NA) # the memory limit
# # memory.size(max = TRUE) # the maximum amount of memory obtained from the OS
# # memory.size(max = FALSE) # the amount currently in use
# # #memory.limit(size = 30000)
# 
# gc() # säubert RAM endgültig von Objekten, welche durch rm() gelöscht wurden
# 
# # Um die Arbeitszeit zu reduzieren, sollen nun die restlichen
# # Kerne freigeschalten werden, da R eine Single Process Language ist und daher
# # nur einen Kern benutzt. Da das Packet proxyC jedoch auf C++ basiert, sollte eine
# # Parallelisierung möglich sein.
# 
# library(parallelMap)
# library(parallel)
# 
# tic("Dauer Jaccard mit proxyC")
# parallelStartSocket(cpus = detectCores())
# 
# JaccardSparse <- proxyC::simil(AuthMat0sparse, method = "jaccard", drop0 = TRUE)
# 
# parallelStop()
# toc()
# 
# JaccardSparse[1,0]
# #Dauer: 6834.2 Sekunden
# # Beim zweiten mal hat es jetzt 7972 Sekunden gedauert!
# 
# object.size(JaccardSparse)
# #Größe: 962310984 bytes = ca. 0.9 GB
# # Zudem ist die Datei auch noch klener geworden und hat nun 648259360 byte also ca. 0,65 GB
# 
# # Hat funktioniert! Solange wir nicht mehr als 16 GB RAM zur Verfügung haben
# # müssen wir manche Similarities (bspw. die mit einem Wert von 0)
# # unberücksichtigt lassen.
# # Um diese nicht immer wieder neu berechnen zu müssen, exportieren wir sie und
# # laden das File anschließend wieder rein.
# 
# DimNames <- dimnames(JaccardSparse)
# 
# writeMM(JaccardSparse, file = "JaccSimOhneNullen.mtx")
# # ACTHUNG: Liegt nicht im Data-Ordner!
# # Leider kann das File nicht auf GitHub hochgeladen werden, da es das Filesize
# # Limit von 100 MB überschreitet.
# 
# JaccSimOhneNullen <- readMM("./Data/JaccSimOhneNullen.mtx")
# 
# all.equal(JaccardSparse, JaccSimOhneNullen)
# str(JaccardSparse)
# str(JaccSimOhneNullen)
# # Leider sind die Matrizen nicht identisch, da die Dimensionnames fehlen!
# # Außerdem ist das Attribut "uplo" ebenfalls verschieden.
# 
# JaccardSparse["'Nathan Burgoine",1:5]
# JaccSimOhneNullen["'Nathan Burgoine",1:5]
# # Das mit den Dimensionnames ist besonders ärgerlich, da sich so nicht mehr die 
# # Autoren als Character adressieren lassen.
# 
# #ColRowNam <- list(Authors,Authors)
# #dimnames(JaccSimOhneNullen) <- ColRowNam
# # Funktionert nicht. U.a. vermutlich auch deshalb, weil die Dimensionen/Längen
# # unterschiedlich sind!
# 
# DimNamesJacc <- dimnames(JaccardSparse)
# # Ist wesentlich kürzer als ColRowNam. Vermutung: Eventuell sind manche Autoren 
# # herausgefallen, weil sie keine Similarity zu irgendjemandem hatten?
# 
# dimnames(JaccSimOhneNullen) <- DimNamesJacc
# str(JaccSimOhneNullen)
# # Hat funktioniert! Der Workaroudn funktioniert, ist aber alles andere als
# # zufriedenstellend, außerdem ist immer noch unklar, was es mit dem Attribut
# # uplo auf sich hat und warum es in der "Originalmatrix" U ist und in der
# # exportierten/importierten L
# 
# rm(JaccardSparse)
# gc()

## Anwendung Cosine - akutelle Alternative, da schneller #######################
library(parallelMap)
library(parallel)

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

DimNamesCos <- dimnames(CosineSparse)

writeMM(CosineSparse, file = "CosineSimOhneNullen.mtx")
# ACHTUNG: Das File liegt nicht im Order Data sondern im Oberordner!
# Leider kann das File nicht auf GitHub hochgeladen werden, da es das Filesize
# Limit von 100 MB überschreitet.

CosineSimOhneNullen <- readMM("./Data/CosineSimOhneNullen.mtx")
dimnames(CosineSimOhneNullen) <- DimNamesCos

### Similarity Weighting gemäß Herlocker et al.
# Im Prinzip erstellt man eine Matrix welche den Dimensionen der AuthMat0sparse-Matrix entspricht
# Also Zeilen und Spalten etnsprechen der Anzahl der Autoren.
# Anschließend wird ermittelt, wie viele Main Topics die Autoren gemeinsam haben
# Überschreitet diese Anzahl den Schwellenwert, so wird der Schwellenwert durch sich selbst geteilt,
# was wiederum 1 ergibt. Wird diese Matrix dann elementweise mit der CosineSim-Matrix multipliziert,
# wird die Cosine similarity NICHT bestraft.

# Unterschreitet die Anzahl der gemeinsamen Main-Topics jedoch den Schwellenwert, dann wird diese
# Zahl durch den Schwellenwert geteilt, was wiederum kleiner als 1 ist.
# Wird diesser Wert dann elementweise mit der CosineSim-Matrix multipliziert,
# wird die Cosine similarity NICHT bestraft.

# Ob dieser Ansatz jedoch Sinn macht, ist fraglich, da sich die Idee von Filmen
# als einzelne Items nicht hundertprozentig auf ganze Genre übertragen lässt.
# Hat sich ein Autor bspw. auf ein Buch spezialisiert und 30 Bücher in diesem
# Genre geschrieben und trifft nun auf einen Autor, der ebenfalls 30 Bücher in
# diesem Genre geschrieben hat, dann haben wir eine perfekte Similartiy.
# Allerdings würde diese unnötigerweise bestraft, wenn wir einen Schwellenwert
# größer als 1 setzen.

SignWeightMat <- matrix(ncol=nrow(AuthMat0sparse), nrow=nrow(AuthMat0sparse))

SchwelleMutuals <- 5

tic("Dauer Berechnung Similarity Penalty nach Herlocker")
parallelStartSocket(cpus = detectCores())

for (i in 1:nrow(AuthMat0sparse)){
  for (j in 1:nrow(AuthMat0sparse)) 
    SignWeightMat[i,j] <- ifelse(sum( (AuthMat0sparse[i,] != 0) + (AuthMat0sparse[j,] != 0) ) < SchwelleMutuals,  
                                    sum( (AuthMat0sparse[i,] != 0) + (AuthMat0sparse[j,] != 0) )  / SchwelleMutuals, 
                                    SchwelleMutuals/SchwelleMutuals)
}

diag(MutualsGewMatrix) <- 1

parallelStop()
toc()

###

sum( (AuthMat0sparse[1,] != 0) + (AuthMat0sparse[2,] != 0) )

## ALT - Tests mit Recommenderlab#####################################################

# # Ansatz über Recommenderlab - wir betrachten diese Matrix einfach als RatingMatrix
# # D.h. wir tun so, als hätten wir User und Ratings ... faktisch aber haben wir ja
# # Autoren und Main Topics. Letztlich ist es auch egal, wie die Inhalte interpretiert
# # werden, wichtig ist, dass sparse Matrizen verarbeitet werden können.
# 
# doc1 <- c(3,2,1,2,2,0,0)
# doc2 <- c(2,1,0,1,2,0,0)
# doc3 <- c(0,0,0,1,2,3,2)
# doc4 <- c(0,0,1,0,1,2,3)
# test <- rbind(doc1, doc2, doc3, doc4)
# 
# binRatMat <- as(test, "binaryRatingMatrix")
# realRatMat <- as(test, "realRatingMatrix")
# 
# recommenderlab::similarity(binRatMat, method = "Jaccard")
# recommenderlab::similarity(realRatMat, method = "Jaccard")
# recommenderlab::similarity(realRatMat, method = "Cosine")
# 
# # Test, ob es einen mathematischen Unterschied zwischen 0 und NA gibt
# 
# doc1 <- c(3,2,1,2,2,NA,NA)
# doc2 <- c(2,1,NA,1,2,NA,NA)
# doc3 <- c(NA,NA,NA,1,2,3,2)
# doc4 <- c(NA,NA,1,NA,1,2,3)
# 
# testRM <- rbind(doc1, doc2, doc3, doc4, doc5, doc6, doc7, doc8)
# testRM <- as(testRM, "realRatingMatrix")
# 
# recommenderlab::similarity(testRM, method = "Jaccard")
# recommenderlab::similarity(testRM, method = "Cosine")
# 
# # Testergebnis: Leider gibt es einen Unterschied. Gerade bei Jaccard kommen dabei
# # nur noch Werte von 1 raus, die eigentlich nicht mehr interpretierbar sind.
# # D.h. die NA Werte müssten eigentlich durch Nullen ersetzt werden.
# 
# SimMatTri <- recommenderlab::similarity(realRatMat, method = "Jaccard")
# SimMatTot <- as.matrix(recommenderlab::similarity(realRatMat, method = "Jaccard"))
# 
# #SimMatTri[doc1] # Das Dreieck funktioniert nicht, man braucht die komplette Matrix
# SimMatTot["doc1",]
# 
# # Sieht gut aus!

## ALT - Anwendung von Recommenderlab#################################################

# AuthMatRatMat <- as(AuthMat, "realRatingMatrix")
# 
# rm(AuthMat)
# rm(AuthMat0)
# 
# # recommenderlab::similarity(realRatMat, method = "Jaccard")
# # recommenderlab::similarity(realRatMat, method = "Cosine")
# 
# tic("Dauer Jaccard Complete")
# AuthorJacc <- as.matrix(recommenderlab::similarity(AuthMatRatMat, method = "Jaccard"))
# toc()
# 
# tic("Dauer Jaccard Triangle")
# AuthorJaccTri <- recommenderlab::similarity(AuthMatRatMat, method = "Jaccard")
# toc()
# 
# #diag(AuthorJacc) <- 1
# 
# #AuthorCos <- as.matrix(proxy::simil(AuthMat0, method = "cosine"))
# #diag(AuthorCos) <- 1


#################################################################################

# Somit ist Phase 1 - die Ermittlung der Ähnlichkeite der Autoren basierend auf 
# den Main Topics - abgeschlossen.
# Im nächsten Schritt müssen wir uns die Zielautoren ausgeben lassen,
# anschließend können wir in der Matrix nach besagtem Autor suchen und uns
# dessen kNearestNeighbors sowie deren geschriebene Bücher ausgeben lassen.

# Laden des Evaluationsdatensatz
evaluation <- read.csv("./Data/evaluation.csv")

# Verknüfung von Evaluation mit den Items, um die Autorennamen zuzuordnen
items5 <- read.csv("./Data/items5.csv")

activeAuthorAndTitel <- evaluation %>% 
  left_join(items5, by = "itemID") %>% 
  select(itemID, author, title)

#rm(items5)
rm(evaluation)

## Jaccard-KNN - auskommentiert ################################################

# activeAuthor1 <- activeAuthorAndTitel$author[1]
# aA1_NeighorsJacc <- JaccSimOhneNullen[activeAuthor1,]
# aA1_top10NeigbhorsJacc <- sort(aA1_Neighors, decreasing = TRUE)[1:10]
# aA1_top10NeigbhorsJacc
# length(aA1_NeighorsJacc[aA1_NeighorsJacc == 1])
# hist(aA1_NeighorsJacc, main = "aA1_NeighorsJacc")
# summary(aA1_NeighorsJacc)
# # Wie man sieht, scheint der Jaccard Coefficient nicht sensibel genug zu sein,
# # da es 3674 Autoren gibt, die eine Similarity mit dem aktiven Autor von 1 haben
# # Insofern werden wir wohl auf die Cosine Similarity zurückgreifen müssen.
# 
# rm(JaccSimOhneNullen)
# gc()

## Cosine-KNN ####################################################################

activeAuthor1 <- activeAuthorAndTitel$author[1]
aA1_NeighorsCos <- CosineSimOhneNullen[activeAuthor1,]
aA1_top10NeigbhorsCos <- sort(aA1_NeighorsCos, decreasing = TRUE)[1:10] # Namen der Autoren und deren Similarities
aA1_top10NeigbhorsCos <- names(aA1_top10NeigbhorsCos) # nur noch deren Namen
length(aA1_NeighorsCos[aA1_NeighorsCos == 1])
hist(aA1_NeighorsCos, main = "aA1_NeighorsCos")
summary(aA1_NeighorsCos)
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

# Zunächst: Extrahieren des Zielbuchtitels:
activeTitel1 <- activeAuthorAndTitel$title[1]
activeTitel1

# Bevor man diese diese nun für Text Mining verwenden kann, muss man eine doc_id
# in Spalte 1 hinzufgen und die Spalte 2 in 'text' umbenennen

# Extrahieren aller Buchtitel des Ziel-Autors, außer des Ziel-Titels
allTitelActAuth1 <- items5 %>% 
  select(itemID, author, title) %>% 
  filter(author == activeAuthor1 & title != activeTitel1) %>% 
  select(itemID, title) %>% 
  rename(doc_id = itemID, text = title)

# Extrahieren aller Buchtitel der kNN
allTitelKNN <- items5 %>% 
  select(itemID, author, title) %>% 
  filter(str_detect(.$author, paste(aA1_top10NeigbhorsCos, collapse = "|"))) %>% 
  select(itemID, title) %>% 
  rename(doc_id = itemID, text = title)

# Jetzt müssen noch das Ziel-Buch, die Titel des Autoren und die der kNN
# zusammengeführt und die doc_id entsprechend vergeben werden

activeTitel1 <- items5 %>% 
  select(itemID, title) %>% 
  filter(title == activeTitel1) %>% 
  rename(doc_id = itemID, text = title)

allTitles <- rbind(activeTitel1, allTitelActAuth1, allTitelKNN)

library(tm)

Corpus <- VCorpus(DataframeSource(allTitles))
Corpus

# Empfohlenes Pre-Processing aus 'Modern Data Science with R'
Corpus <- Corpus %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) # %>% tm_map(removeWords, stopwords('german'))
# Kann nicht einfach noch deutsche Stopwords entfernen, da dann englische Wörter wie 'Die'
# ebenfalls gelöscht werden würden :-/

strwrap(as.character(Corpus[[10]]))
# Eigentlich müsste man dan den Items-Datensatz in Sub-Datensätze nach Sprache
# aufteilen, um effektiv Stopwords entfernen zu können.
# Oder man pickt aus den 3500 Neighbors nur die mit der gleichen Sprache raus

## Visualisierung als Wourdcloud

library(wordcloud)
wordcloud(Corpus, max.words = 30, scale = c(8, 1),
          colors = topo.colors(n = 30), random.color = TRUE)
# War nicht wirklich aussagekräftig ;-)

# Als nächstes wird der Corpus in eine TF-IDF-Matrix überführt:
DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
DTM
inspect(DTM)

# Da man eine DTM nicht in eine sparseMatrix transformieren kann - das jedoch
# ein zentrales Kriterium für proxyC ist - müssen wir die cosine-Function aus
# einem anderen Paket benutzen

library(parallelMap)
library(parallel)

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

top5Recc <- sort(CosineTitel[as.character(activeTitel1$doc_id),], decreasing = TRUE)
top5Recc <- top5Recc[names(top5Recc) != as.character(activeTitel1$doc_id)][1:5]
top5Recc
# Am häufigsten wird das Buch sich selbst empfohlen - sonst kein anderes -.-'

items5 %>% filter(stringr::str_detect(title,"Hogwarts")) %>% select(itemID, title, author) %>% View()
