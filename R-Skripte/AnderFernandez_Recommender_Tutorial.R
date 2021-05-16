############ Data Mining Cup ##########

########## Recommender-System (hybrid) ##########

########## Packages ###########
library(tidyverse)
#library(janitor)
library(arules)
library(arulesViz)
library(arulesSequences)
library(cluster)

## Arbeitsverzeichnis
getwd()

########### Daten ###########
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items_bearbeitet4.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
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
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
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
## neues Tibble --> nach itemID zusammengefasste Werte!
tibble_with_ratios <- joined_oR %>%
  group_by(itemID) %>%
  summarise(
    author = author,
    publisher = publisher,
    main_topic = main.topic,
    sum_clicks = sum(click),
    sum_basket = sum(basket),
    sum_order = sum(order),
    mean_click_order_ratio = mean(click_order_ratio),
    mean_basket_order_ratio = mean(basket_order_ratio)
  ) %>%
  distinct(.keep_all = T) %>%
  lapply(factor) %>%
  data.frame()
# Übersicht
summary(tibble_with_ratios)
glimpse(tibble_with_ratios)
head(tibble_with_ratios, n=20)

## Variablen rekodieren in Factor
# joined_oR <- as_tibble(lapply(joined_oR, factor))
# head(joined_oR, n=20)

## an 'numerischen' Factor itemID den Zusatz 'IID' hängen, damit später keine Probleme auftreten beim umwandeln in Zeilen-und Spaltennamen der Matrix
# joined_oR$sessionID <- as.factor(paste0("SID.", joined_oR$sessionID))
tibble_with_ratios$itemID <- as.factor(paste0("IID.", tibble_with_ratios$itemID))
head(tibble_with_ratios, n=20)

## NA-Daten von click, basket und order rasuwerfen
# joined_tbl_onlyOrders <- joined_oR %>%
#   filter(!is.na(order) & order != 0)
# head(joined_tbl_onlyOrders, n=20) # ca 50.000 Zeilen entfernt & etwa 365.000 Zeilen verbleiben

########### Daten inspizieren #############
# # Plot: meiste Autoren
# joined_tbl_onlyOrders %>%
#   group_by(author) %>%
#   summarise(nOrders = sum(as.integer(order), na.rm = T)) %>%
#   filter(nOrders >= 400) %>%
#   ggplot(aes(x = author, y = nOrders)) +
#   geom_col() +
#   theme_minimal()
# 
# Plot: meiste Titel
# tibble_with_ratios %>%
#   group_by(title) %>%
#   summarise(nOrders = sum(as.integer(order), na.rm = T)) %>%
#   filter(nOrders >= 150) %>%
#   ggplot(aes(x = title, y = nOrders)) +
#   geom_col() +
#   theme_minimal()

####### Dissimilarity zwischen den Büchern berechnen #######
## Cluster-Package einladen & auf Similarity zwischen Author, Verlag und MainTopic überprüfen
tibble_with_ratios_ohneNA <- tibble_with_ratios[!is.na(tibble_with_ratios$sum_clicks) ,]
books_features <- tibble_with_ratios_ohneNA[, c("author", "main_topic", "publisher")]
dissimilarity <- daisy(books_features, metric = "gower", weights = c(2, 1, 1))
dissimilarity <- as.matrix(dissimilarity)

## Rownames & Colnames
row.names(dissimilarity) <- joined_tbl_onlyOrders$itemID
colnames(dissimilarity) <- joined_tbl_onlyOrders$itemID
## Similarity der Bücher prüfen --> 0="gleich", 1="komplett unterschiedlich"
dissimilarity[35:40, 35:40]
## Reihenfolge der Recommendations
# 1 - gleicher Titel, gleicher Autor, gleicher Verlag, gleiches MainTopic
# 2 - gleicher Autor, gleiches MainTopic
# 3 - gleicher Autor
# 4 - gleiches MainTopic, gleicher Verlag
# 5 - gleiches MainTopic
# 6 - gleicher Verlag

######### Content-Based Recommendation System bauen #########
# irgendeine ItemID raussuchen als extra Variable definieren
spec_item_id <- "IID.369"
# Merkmale der ItemID in Datensatz rausfiltern und extra abspeichern
spec_item_books <- joined_tbl_onlyOrders %>%
  filter(itemID == spec_item_id)
head(spec_item_books, n=10)

## itemID wieder als Character-Variable rekodieren
joined_tbl_onlyOrders$itemID <- as.character(joined_tbl_onlyOrders$itemID)

## neuen 
selected_books <- spec_item_books[, c("itemID", "author", "publisher", "main.topic")]
recommend <- function(selected_books2, dissimilarity_matrix, books, n_recommendations=5)
  {
  selected_books_index <- which(colnames(dissimilarity_matrix) %in% selected_books2$itemID)
  
  results = data.frame(dissimilarity_matrix[, selected_books_index],
                       recommended_book = row.names(dissimilarity_matrix),
                       stringsAsFactors = F)
  
  recommendations = results %>%
    pivot_longer(cols = c(-"recommended_book"),
                 names_to = "readed_book",
                 values_to = "dissimilarity") %>%
    left_join(selected_books2, by = c("recommended_book" = "itemID")) %>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_book != readed_book) %>%
    filter(!is.na(author) & !is.na(main.topic)) %>%
    mutate(
      similarity = 1-dissimilarity
    ) %>%
    arrange(desc(similarity)) %>%
    filter(similarity>0) %>%
    group_by(recommended_book) %>%
    slice(1) %>%
    top_n(n_recommendations, similarity) %>%
    left_join(joined_tbl_onlyOrders, by = c("recommended_book"="itemID"))

  return(recommendations)
  }

recommendation <- recommend(selected_books, dissimilarity_books, joined_tbl_onlyOrders)
recommendation

######### Cosinus-Ähnlichkeit ###########
cos_similarity <- function(A,B) {
  num = sum(A*B, na.rm=T)
  den = sqrt(sum(A^2, na.rm=T)) * sqrt(sum(B^2, na.rm=T))
  result = num/den
  
  return(result)
}







