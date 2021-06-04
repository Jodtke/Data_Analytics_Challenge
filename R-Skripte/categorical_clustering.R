######### k-means-clustering als numerisches Feature (evtl. für Gewichtung) ###########
library(tidyverse)
library(cluster)
library(NbClust)
library(fclust)
library(ppclust)
library(factoextra)

## Arbeitsverzeichnis
getwd()

########### Daten ###########
items <- read_csv(file="./Data/items5.csv", col_names=T, col_types=cols(
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
joined_item_trans$click_order_ratio <- sapply(joined_item_trans$click_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
## durchschnittliche Basket-Order-Ration pro Item berechnen
joined_item_trans$basket_order_ratio <- joined_item_trans$basket / joined_item_trans$order
joined_item_trans$basket_order_ratio <- sapply(joined_item_trans$basket_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
head(joined_item_trans, n=10)
glimpse(joined_item_trans, n=10)

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
    sum_orders = sum(order, na.rm=T)
  ) %>%
  mutate(
    mean_click_order_ratio = as.factor(mean_click_order_ratio),
    mean_basket_order_ratio = as.factor(mean_basket_order_ratio)
    ) %>%
  distinct(.keep_all=T) %>%
  filter(as.numeric(sum_clicks)>0)
head(tibble_with_ratios, n=20)

## Books Features extra abspeichern
books_features <- data.frame(tibble_with_ratios[1:20000, c("title", "author", "main_topic") ] )
head(books_features, n=10)

## unwichtige datensätze entfernen
rm(items, trans)

######### Distanz-Matrix mit Gower-Distanz erstellen (und daisy-package) ############
features_dist <- daisy(books_features, metric="gower", weights=c(2, 1.5, 1))
features_distMatrix <- as.matrix(features_dist)

## visualize matrix --> kaum möglich mit 211 MB Matrix!!!!
#gradient_color <- list(low="gold", high="black")
#factoextra::fviz_dist(features_dist, gradient=gradient_color, order=T)

## wie viele verschiedene Cluster? --> Silhouette Methode!
# für spanne an möglichen cluster größe wird die Gleichheit von Werten innerhalb der Cluster zueinander vs.
# der Gleichheit ggü. Wert5en außerhalb der spezifischen Cluster gemessen
# score von -1 bis +1 für jedes Cluster ausgegeben --> 1=tolle clusterung der werte innerhalb (gleichheit) & -1=schlechte clusterung der werte innerhalb
# anzahl der cluster mit maximalem score (also höchster gleichheit innerhlab der cluster) wird gewählt für anzahl der cluster

########## silhouette methode ##########
number_clusters <- NbClust(diss=features_dist, distance=NULL, min.nc=2, max.nc=50, method="median", index="silhouette")
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
silhouette <- c()
silhouette = c(silhouette, NA)
for (i in 2:25) {
  pam_clusters = pam(as.matrix(features_dist), diss=T, k=i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}
plot(1:15, silhouette,
      xlab = "Clusters",
      ylab = "Silhouette Width")
lines(1:15, silhouette)

