######### k-means-clustering als numerisches Feature (evtl. für Gewichtung) ###########
library(tidyverse)
library(cluster)
library(NbClust)
library(fclust)
library(ppclust)
library(diceR)
library(factoextra)
library(mlr3)
library(mlr3cluster)
library(stringdist)
library(tokenizers)
library(reshape2)
library(parallelMap)
library(parallel)

## Arbeitsverzeichnis
getwd()

########### Daten ###########
items <- read_csv(file="./Data/items6.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_factor(),
  author=col_factor(),
  publisher=col_factor(),
  main.topic=col_factor(),
  subtopics=col_character()
))
head(items, n=20)
glimpse(items)

trans <- read_delim(file="./Data/transactions.csv", col_names=T, delim="|", col_types=cols(
  sessionID = col_factor(),
  itemID = col_factor(),
  click = col_integer(),
  basket = col_integer(),
  order = col_integer()
))
## joinen & Reihenfolge der Spalten verändern
joined_item_trans <- left_join(items, trans, by="itemID")
joined_item_trans <- joined_item_trans[, c(7,1,8:10,2:6) ]
head(joined_item_trans, n=10)

## durchschnittliche Click-Order-Ratio pro Item berechnen
joined_item_trans$click_order_ratio <- joined_item_trans$click / joined_item_trans$order
joined_item_trans$click_order_ratio <- sapply(X=joined_item_trans$click_order_ratio, FUN=function(x) if_else(x=="Inf" | x=="NaN", 0, x))
## durchschnittliche Basket-Order-Ration pro Item berechnen
joined_item_trans$basket_order_ratio <- joined_item_trans$basket / joined_item_trans$order
joined_item_trans$basket_order_ratio <- sapply(X=joined_item_trans$basket_order_ratio, FUN=function(x) if_else(x=="Inf" | x=="NaN", 0, x))
head(joined_item_trans, n=10)
glimpse(joined_item_trans, n=10)

# evaluation datensatz
evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
evaluation_tbl <- as_tibble(evaluation)
evaluation_tbl$itemID <- as.factor(evaluation_tbl$itemID)

# tibble mit ratios und variablen als factor erstellen
tibble_with_ratios <- joined_item_trans %>%
  group_by(itemID) %>%
  summarise(
    title = title,
    author = author,
    main_topic = main.topic,
    publisher = publisher,
    mean_click_order_ratio = round(mean(click_order_ratio, na.rm=T), digits=2),
    mean_basket_order_ratio = round(mean(basket_order_ratio, na.rm=T), digits=2),
    sum_clicks = sum(click, na.rm=T),
    sum_orders = sum(order, na.rm=T),
    n = n()
  ) %>%
  distinct(.keep_all=T) %>%
  filter(as.numeric(sum_clicks)>0)
head(tibble_with_ratios, n=20)

## Books Features extra abspeichern
books_features <- data.frame(tibble_with_ratios[1:20000, c("title", "author", "publisher", "main_topic", "sum_clicks", "sum_orders") ] )
head(books_features, n=10)

## unwichtige datensätze entfernen
rm(items, trans, evaluation)

######### Distanz-Matrix mit Gower-Distanz erstellen (mit daisy-befehl) ############
features_dist <- daisy(books_features, metric="gower", weights=c(2, 2, 1, 1.5, 1, 1), type=list(ordratio=c(5,6)))
features_distMatrix <- as.matrix(features_dist)

### Paare finden in books features DataFrame
summary(features_dist) # viele unähnlichkeiten, im 1.Quantil ganze 76% Dissimilarity ):
books_features[ which(
    features_distMatrix==min(features_distMatrix[ features_distMatrix!=min(features_distMatrix) ] ), arr.ind=T) [1, ], ]

## visualize matrix --> kaum möglich mit 211 MB Matrix!!!!
#gradient_color <- list(low="gold", high="black")
#factoextra::fviz_dist(features_dist, gradient=gradient_color, order=T)

## wie viele verschiedene Cluster? --> Silhouette Methode!
# für spanne an möglichen cluster größe wird die Gleichheit von Werten innerhalb der Cluster zueinander vs.
# der Gleichheit ggü. Wert5en außerhalb der spezifischen Cluster gemessen
# score von -1 bis +1 für jedes Cluster ausgegeben --> 1=tolle clusterung der werte innerhalb (gleichheit) & -1=schlechte clusterung der werte innerhalb
# anzahl der cluster mit maximalem score (also höchster gleichheit innerhlab der cluster) wird gewählt für anzahl der cluster

########## silhouette methode ##########
number_clusters <- NbClust(diss=features_dist, distance=NULL, min.nc=2, max.nc=25, method="median", index="silhouette")
number_clusters_daisy <- NbClust(diss=features_dist, distance=NULL, min.nc=2, max.nc=30, method="centroid", index="dunn")
# wenn keine numerischen Gewichtungsfaktoren in Distanzmatrix einberechnet werden 6 Cluster empfohlen
# wenn numerische Variablen inbegriffen (=Ratios!) werden nur 2 Cluster empfohlen
# Vermutung Eric: wahrscheinlich besitzen so wenige Items überhaupt eine zu berechnende Ratio, dass an dieser Stelle nahezu binäre 0 und >0 Entscheidung
# zur Clusterung in nur 2 Cluster getroffen wird
# mögliche Verbesserung: Gewichtung der Ratios verringern!

# PAM anwenden
features_pam <- pam(features_distMatrix, 6)

# visualize pam
features_mds6 <- as.data.frame(cmdscale(features_dist, 6))
features_mds6 <- features_mds6 %>% rename(disCluster1=V1, disCluster2=V2, disCluster3=V3, disCluster4=V4, disCluster5=V5, disCluster6=V6)
features_mds6$features_cluster <- as.factor(features_pam$clustering)
#### Achtung Variablen umbauen
features_mds6$itemID <- books_features$itemID # itemId an features_mds6 hängen
books_withClusters <- books_features # neuen Datensatz bauen für joining
books_withClusters <- left_join(x=books_withClusters, y=features_mds6, by="itemID")
head(books_withClusters, n=20)

# plot zu groß ):
ggplot(books_withClusters, aes(x=disCluster1, y=disCluster3, color=features_cluster)) + 
  geom_point() +
  theme_minimal() +
  labs(title="Cluster Plot for Similarities",
       subtitle="Colored by PAM cluster") +
  scale_color_brewer(palette="Set1")

# umwandlung von books_withClusters
books_withClusters <- books_withClusters %>%
  mutate(
    disCluster1 = round(books_withClusters$disCluster1, digit=2),
    disCluster2 = round(books_withClusters$disCluster2, digit=2),
    disCluster3 = round(books_withClusters$disCluster3, digit=2),
    disCluster4 = round(books_withClusters$disCluster4, digit=2),
    disCluster5 = round(books_withClusters$disCluster5, digit=2),
    disCluster6 = round(books_withClusters$disCluster6, digit=2),
    cluster = as.factor(books_withClusters$features_cluster)
  )
head(books_withClusters, n=20)

# umwandlung von feature_mds6
features_mds6 <- features_mds6 %>%
  mutate(
    disCluster1 = round(features_mds6$disCluster1, digit=2),
    disCluster2 = round(features_mds6$disCluster2, digit=2),
    disCluster3 = round(features_mds6$disCluster3, digit=2),
    disCluster4 = round(features_mds6$disCluster4, digit=2),
    disCluster5 = round(features_mds6$disCluster5, digit=2),
    disCluster6 = round(features_mds6$disCluster6, digit=2),
    cluster = as.factor(features_mds6$features_cluster)
  )
head(features_mds6, n=20)

########### zweiter Ansatz ############
silhouette <- c(NA)
for (i in 2:50) {
  pam_clusters = pam(features_dist, diss=T, k=i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}
plot(2:50, silhouette,
      xlab = "Clusters",
      ylab = "Silhouette Width")
lines(2:50, silhouette)



############## fuzzy c-means clustering ##############
# library(mlr3) # wichtige mlr3 bibliotheken
# library(mlr3cluster)
# mlr_learners$keys("clust") # mögliche cluster-algorithmen, zB c-fuzzy, k-medoids, k-means
# mlr_measures$keys("clust")
# learner = mlr_learners$get("clust.kmeans") # einen Lerner definieren
# learner$param_set # parameter-sets für die hyperparameter-bestimmung
# learner$param_set$values = list(centers = 3L, algorithm = "Lloyd", iter.max = 100L) # zum beispiel...

########## stringdist ###########

### doppelt sessions und sessions mit min 2 Büchern 
dup.ses <- joined_item_trans[joined_item_trans$sessionID %in%
                               joined_item_trans$sessionID[duplicated(joined_item_trans$sessionID) ] ,] # das sind knapp 183k sessions
length(unique(dup.ses$itemID)) # für 67896 Bücher würde das funktionieren
## prüfen wie viel Bücher von denen, die in transaction DS drin sind, auch in evaluation DS erscheinen.
#O b das Sinn macht der Algorithmus zu basteln und der Ansatz zu implimentieren 
pruff_ses <- right_join(dup.ses, evaluation_tbl, by = "itemID")
# view(pruff_ses %>% group_by(itemID))
length(unique(pruff_ses$itemID))
pruff_ses <- drop_na(pruff_ses)  
pruff_ses %>% group_by(itemID) %>% summarise(n=n()) %>% arrange(desc(n)) # 304 Bücher in DS evaluation
# einzelne items (ohne duplikate) aus ludmilas itemliste extrahieren
ludmilas_titles <- pruff_ses %>%
  group_by(itemID) %>%
  summarise(title = title) %>%
  distinct(.keep_all = T) %>%
  mutate(title = as.character(title)) %>%
  as.data.frame()
ludmilas_titles <- ludmilas_titles[,-1]
head(ludmilas_titles, n=20) # 301 items in ludmilas liste enthalten

# 'stringdist', 'stringdistmatrix' oder 'amatch' mögliche Befehle
string_features1 <- as.data.frame(tibble_with_ratios[, "title" ] ) # 24620 unterschiedliche Titel...1
string_features1$titlename <- as.character(string_features1$title) # neue variable mit titlenames als character encoding anfügen
string_features1 <- string_features1[,-1]
head(string_features1, n=20)

# parallelisierung
parallelStartSocket(cpus = detectCores())
title_distances_lv <- stringdistmatrix(a=string_features1, b=ludmilas_titles, # Methode 'lv' = Levenstein-Clusterung
                              method="lv", useBytes=F, useNames=T) ## merkwürdiges Ergebnis, stimmen nur die Länge der Title überein?!
title_distances_qgrams <- stringdistmatrix(a=string_features1, b=ludmilas_titles,
                                                  method="qgram", q=5, # gqgram = 1, jeder einzelne buchstabe der strings wird miteinander vergleichen un die länge der übereinstimmung ausgegeben
                                                  useBytes=F, useNames=T)

### Distance-File to DataFrame Function
title_distances_qgrams_matrix <- melt(as.matrix(title_distances_qgrams) ) # konvertierung der distance-matrix in "molten" dataFrame mit melt() aus package reshape2
p1 <- apply(title_distances_qgrams_matrix[, 1:2 ], MARGIN=1, FUN=function(x) sort(x, decreasing=T))
p1 <- t(p1) # matrix p transponieren um duplikate der diagonale zu entfernen
rmv1 <- which(p1[,1] == p1[,2])
p2 <- paste0(p1[,1], p1[,2], sep="|")
rmv2 <- which(duplicated(p2))
title_distances_qgrams_matrix_oD <- title_distances_qgrams_matrix[-c(rmv1,rmv2) ,] # eigene distanzen und doppelte distanzen entfernen

### Min-Max-Normierung der Daten vornehmen
min_max_norm <- function(x) {
  (x-min(x)) / (max(x)-min(x))
}
title_distances_qgrams_matrix_oD$value <- min_max_norm(title_distances_qgrams_matrix_oD$value) # normierungsfunktion auf "value" variable anwenden
head(title_distances_qgrams_matrix_oD, n=10)
summary(title_distances_qgrams_matrix_oD) # je geringer die Werte hinter den Titelpaaren, desto ähnlicher sind die titel auf Basis der Buchstabane-2er-Kombinationen
#write_csv(x=title_distances_qgrams_matrix_oD, file="title_distances_qgrams_matrix") # leider 512 MB Datei
#distance_matrix <- read_csv(file="title_distances_qgrams_matrix") # einlesen funktioniert


