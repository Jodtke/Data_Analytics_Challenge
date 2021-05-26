######### k-means-clustering als numerisches Feature (evtl. für Gewichtung) ###########
library(tidyverse)
library(cluster)
library(NbClust)
library(parallel)

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
  filter(as.numeric(sum_orders)>0)
head(tibble_with_ratios, n=20)

## Books Features extra abspeichern
books_features <- data.frame(tibble_with_ratios[, c("itemID", "author", "main_topic", "publisher", "mean_click_order_ratio", "mean_basket_order_ratio") ] )
head(books_features, n=10)

## unwichtige datensätze entfernen
rm(items, trans, tibble_with_ratios)

######### Distanz-Matrix mit Gower-Distanz erstellen (und daisy-package) ############
features_dist <- daisy(books_features, metric="gower", weights=c(0.5, 1.25, 1.5, 0.25, 1, 0.75))
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
number_clusters <- NbClust(diss=features_dist, distance=NULL, min.nc=2, max.nc=25, method="median", index="silhouette", alphaBeale=0.1)

# PAM anwenden
features_pam <- pam(features_distMatrix, 2)

# visualize pam
features_mds <- as.data.frame(cmdscale(features_dist, 2))
features_mds <- features_mds %>% rename(disCluster1=V1, disCluster2=V2)
features_mds$features_cluster <- as.factor(features_pam$clustering)
ggplot(features_mds, aes(x=disCluster1, y=disCluster2, color=features_cluster)) + 
  geom_point() +
  theme_minimal() +
  labs(title="Cluster Plot for Similarities",
       subtitle="Colored by PAM cluster") +
  scale_color_brewer(palette="Set1")

# interpretation
books_features <- books_features %>%
  mutate(
    disCluster1 = round(features_mds$disCluster1, digit=2),
    disCluster2 = round(features_mds$disCluster2, digit=2),
    cluster = as.factor(features_mds$features_cluster)
  )
head(books_features, n=20)

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

