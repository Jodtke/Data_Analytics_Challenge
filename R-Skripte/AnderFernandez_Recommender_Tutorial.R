################ Data Mining Cup ##################

########## Recommender-System (hybrid) ##########

########## Packages ###########
library(tidyverse)
#library(janitor)
#library(arules)
#library(arulesViz)
#library(arulesSequences)
library(cluster)

## Arbeitsverzeichnis
getwd()

########### Daten ###########
transactions_raw <- read.csv(file = "./Data/transactions.csv", header=T, sep="|", quote="", row.names=NULL, stringsAsFactors=F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items_bearbeitet4.csv", header=T, sep=",", row.names=NULL, stringsAsFactors=F, encoding="UTF-8")
dim(openRefine)

######### Daten bearbeiten ############
## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions_raw)
glimpse(transactions_tbl)
head(transactions_tbl, n=10)
oR_tbl <- as_tibble(openRefine)
glimpse(oR_tbl)
head(oR_tbl, n=10)

## Joining
joined_oR <- left_join(oR_tbl, transactions_tbl, by="itemID")
glimpse(joined_oR)
head(joined_oR, n=20)
## Reihenfolge der Spalten verändern
joined_oR <- joined_oR[, c(7,1,8:10,2:6) ]
head(joined_oR, n=10)

## durchschnittliche Click-Order-Ratio pro Item berechnen
joined_oR$click_order_ratio <- joined_oR$click / joined_oR$order
joined_oR$click_order_ratio <- sapply(joined_oR$click_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
## durchschnittliche Basket-Order-Ration pro Item berechnen
joined_oR$basket_order_ratio <- joined_oR$basket / joined_oR$order
joined_oR$basket_order_ratio <- sapply(joined_oR$basket_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
head(joined_oR, n=10)
glimpse(joined_oR, n=10)
## neues Tibble --> nach itemID zusammengefasste Werte!
tibble_with_ratios <- joined_oR %>%
  group_by(itemID) %>%
  summarise(
    author = author,
    publisher = publisher,
    main_topic = main.topic,
    sum_clicks = sum(click, na.rm=T),
    sum_baskets = sum(basket, na.rm=T),
    sum_orders = sum(order, na.rm=T),
    mean_click_order_ratio = mean(click_order_ratio),
    mean_basket_order_ratio = mean(basket_order_ratio)
  ) %>%
  distinct(.keep_all=T) %>%
  data.frame()
# Übersicht
head(tibble_with_ratios, n=20)
glimpse(tibble_with_ratios)
summary(tibble_with_ratios)

## an 'numerischen' Factor itemID den Zusatz 'IID' hängen, damit später keine Probleme auftreten beim umwandeln in Zeilen-und Spaltennamen der Matrix
tibble_with_ratios$itemID <- as.factor(paste0("IID.", tibble_with_ratios$itemID))
tibble_with_ratios[, c(2:4) ] <- lapply(tibble_with_ratios[, c(2:4) ], FUN = factor)
head(tibble_with_ratios, n=20)

# unnötige datensätze löschen
rm(openRefine, oR_tbl, transactions_raw, transactions_tbl)

####### Dissimilarity zwischen den Büchern berechnen #######
## Cluster-Package einladen & auf Similarity zwischen Author, Verlag und MainTopic überprüfen mit 'Gower'-Distanz
# nur mit 10.000 Beobachtungen
books_features <- tibble_with_ratios[1:20000, c("author", "publisher", "main_topic") ]
head(books_features, n=10)
dissimilarityMatrix <- daisy(data.frame(books_features), metric = "gower", weights = c(2, 1.5, 0.5))
dissimilarityMatrix <- as.matrix(dissimilarityMatrix)
feature_ids <- tibble_with_ratios[1:20000, c("itemID") ]

## Rownames & Colnames
row.names(dissimilarityMatrix) <- feature_ids
colnames(dissimilarityMatrix) <- feature_ids
## Similarity der Bücher prüfen --> 0="gleich", 1="komplett unterschiedlich"
dissimilarityMatrix[35:40, 35:40]
## Reihenfolge der Recommendations
# 1 - gleicher Titel, gleicher Autor, gleicher Verlag, gleiches MainTopic
# 2 - gleicher Autor, gleiches MainTopic
# 3 - gleicher Autor
# 4 - gleiches MainTopic, gleicher Verlag
# 5 - gleiches MainTopic
# 6 - gleicher Verlag

######### Content-Based Recommendation System bauen #########
# irgendeine ItemID raussuchen als extra Variable definieren
spec_book_id <- "IID.369"
# Merkmale der ItemID in Datensatz rausfiltern und extra abspeichern
spec_books <- tibble_with_ratios %>% filter(itemID == spec_book_id)
head(spec_books)

## itemID wieder als Character-Variable rekodieren
tibble_with_ratios$itemID <- as.character(tibble_with_ratios$itemID)
tibble_with_ratios_offNAs <- tibble_with_ratios %>% filter(!is.na(mean_click_order_ratio) & mean_click_order_ratio>0)
glimpse(tibble_with_ratios_offNAs)
head(tibble_with_ratios_offNAs)
summary(tibble_with_ratios_offNAs)

## neuen 
spec_selected_books <- spec_books[, c("itemID", "mean_click_order_ratio") ]
recommend <- function(selected_books, dis_matrix, books, n_recommendations=5)
  {
  selected_books_index <- which(colnames(dis_matrix) %in% selected_books$itemID)
  
  results = data.frame(dis_matrix[, selected_books_index],
                       recommended_book = row.names(dis_matrix),
                       stringsAsFactors=F)
  
  recommendations = results %>%
    pivot_longer(cols = c(-"recommended_book"), names_to = "readed_book", values_to = "dissimilarity") %>%
    left_join(selected_books, by = c("recommended_book" = "itemID")) %>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_book != readed_book) %>%
    mutate(
      similarity = 1 - as.numeric(dissimilarity),
      weighted_similarity = as.numeric(similarity) * as.numeric(mean_click_order_ratio)
    ) %>%
    filter(similarity>0) %>%
    select(-2) %>%
    group_by(recommended_book) %>%
    arrange(desc(similarity))

  return(recommendations)
  }

recommendation <- recommend(spec_selected_books, dissimilarityMatrix, tibble_with_ratios_offNAs[1:20000])
head(recommendation)

######### Cosinus-Ähnlichkeit ###########
cos_similarity <- function(A,B) {
  num = sum(A*B, na.rm=T)
  den = sqrt(sum(A^2, na.rm=T)) * sqrt(sum(B^2, na.rm=T))
  result = num/den
  
  return(result)
}





