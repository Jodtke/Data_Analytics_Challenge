############ Data Mining Cup ##########

###
##Rules with sessionID
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
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

evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
evaluation_tbl <- as_tibble(evaluation)
evaluation_tbl$itemID <- as.factor(evaluation_tbl$itemID)
dim(evaluation)

rm(items, trans, evaluation) # ursprüngliche dateien wieder löschen!

#abspeichern nur die sessions, die >= 2 Bücher hatten
dup.ses <- joined_item_trans[joined_item_trans$sessionID %in%
                   joined_item_trans$sessionID[duplicated(joined_item_trans$sessionID) ] ,] # das sind knapp 183k sessions

length(unique(dup.ses$itemID)) #für 67896 Bücher würde das funktionieren

##prüfen wie viel Bücher von denen, die in transaction DS drin sind, auch in evaluation DS erscheinen.
#Ob das Sinn macht der Algorithmus zu basteln und der Ansatz zu implimentieren 
pruff_ses <- right_join(dup.ses, evaluation_tbl, by = "itemID")
#view(pruff_ses %>% group_by(itemID))
length(unique(pruff_ses$itemID))
pruff_ses <- drop_na(pruff_ses)  
pruff_ses %>% group_by(itemID) %>% summarise(n=n()) %>% arrange(desc(n)) # 304 Bücher in DS evaluation
##############################################

#z.b. wir suchen Recomendation für item_44827
item <- 44827
this_ses <- dup.ses %>% group_by(sessionID) %>%    #aus dem Datensatz, wo nur dupplicated sessions drin
            filter(itemID==item) %>% select(sessionID)
this_ses <- this_ses$sessionID
this_books_tbl <- dup.ses %>% filter(sessionID %in% this_ses) %>% arrange(sessionID) 
#this_books <- this_books %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
              #summarise(n=n()) %>% arrange(desc(n)) %>% filter(n == max(n))
#
this_books_tbl
amount_ofbooks <- this_books_tbl %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
                   summarise(n=n()) %>% arrange(desc(n))
amount_ofbooks
top5 <- this_books_tbl %>% group_by(itemID) %>% filter(itemID != item)%>% 
        summarise(nClick=sum(click),nBasket=sum(basket),norder=sum(order)) %>%
        arrange(desc(nClick))
top5 <- slice_head(top5, n = 5)
top5 <- top5$itemID
top5
###########################
######alternative##########
recommended_session <- this_books_tbl[[1,1]]
list_of_recommendations <- dup.ses %>% filter(sessionID==recommended_session)
View(list_of_recommendations)

#Recommendation Function based on sessionID
#für 1 Buch erstmal 
Recommendation_function <- function(active_book){
  this_ses <- dup.ses %>% group_by(sessionID) %>%   
              filter(itemID==active_book) %>% select(sessionID)
  this_ses <- this_ses$sessionID
  this_books_tbl <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID) 
  top5 <- this_books_tbl %>% group_by(itemID) %>% filter(itemID != active_book)%>% 
    summarise(nClick=sum(click),nBasket=sum(basket),norder=sum(order)) %>%
    arrange(desc(nClick))
  
  top5 <- slice_head(top5, n = 5)
  
  top5 <- top5$itemID
  return(top5)
}

#try
Recommendation_function(5468)

#lassen die Funktion für ein Vektor der Bücher laufen
books <- c(5468,44827)
result <- list()
for (i in 1:length(books)) {
  book <- books[i] 
    result[[i]] <- Recommendation_function(book)
}
result

