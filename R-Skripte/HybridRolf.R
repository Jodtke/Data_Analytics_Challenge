############ Data Mining Cup ##########

###
##Rules with sessionID
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
transactions_raw <- read.csv(file = "C:/Users/Rolf/Desktop/WFI - Unterlagen/Master/4. Semester/Data Analytics Challenge/Data_Analytics_Challenge/Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
#dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "C:/Users/Rolf/Desktop/WFI - Unterlagen/Master/4. Semester/Data Analytics Challenge/Data_Analytics_Challenge/Data/items5.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
#dim(openRefine) #78334 X 6
evaluation <-  read.csv(file = "C:/Users/Rolf/Desktop/WFI - Unterlagen/Master/4. Semester/Data Analytics Challenge/Data_Analytics_Challenge/Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
#dim(evaluation)

## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions_raw)
#dim(transactions_tbl)
oR_tbl <- as_tibble(openRefine)
#dim(oR_tbl)
evaluation_tbl <- as_tibble(evaluation)

#join the files
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
#Reifolge der Spalten verÃ¤ndern 
joined_oR <- select(joined_oR, c(7:10,1,2,5,3,4,6))

#abspeichern nur die sessions, die >= 2 BÃ¼cher hatten
dup.ses <- joined_oR[joined_oR$sessionID %in%
                       joined_oR$sessionID[duplicated(joined_oR$sessionID)],] # das sind knapp 183k sessions

length(unique(dup.ses$itemID)) #fÃ¼r 67896 BÃ¼cher wÃ¼rde das funktionieren

##prÃ¼fen wie viel BÃ¼cher von denen, die in transaction DS drin sind, auch in evaluation DS erscheinen.
#Ob das Sinn macht der Algorithmus zu basteln und der Ansatz zu implimentieren 
pruff_ses <- right_join(dup.ses, evaluation_tbl, by = "itemID")
#view(pruff_ses %>% group_by(itemID))
length(unique(pruff_ses$itemID))
pruff_ses <- drop_na(pruff_ses)  
pruff_ses %>% group_by(itemID) %>% summarise(n=n()) # 304 BÃ¼cher in DS evaluation
##############################################

#z.b. wir suchen Recomendation fÃ¼r item_44827
item <- 44827
this_ses <- dup.ses %>% group_by(sessionID) %>%    #aus dem Datensatz, wo nur dupplicated sessions drin
  filter(itemID==item) %>% select(sessionID)
this_ses <- this_ses$sessionID
this_books_tbl <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID) 
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
recommended_session <- this_books[[1,1]]
list_of_recommendations <- dup.ses %>% filter(sessionID==recommended_session)
View(list_of_recommendations)

#Recommendation Function based on sessionID
#fÃ¼r 1 Buch erstmal 
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

#lassen die Funktion fÃ¼r ein Vektor der BÃ¼cher laufen
books <- c(5468,44827)
result <- list()
for (i in 1:length(books)){
  book <- books[i] 
  result[[i]] <- Recommendation_function(book)
}
result

## Ab hier: Herausfinden, welche Items, Autoren und Topics gemeinsam in
# Transaktionen waren. Dafür hab ich folgende Lösungen von Github erarbeitet:

## Topic Pairs

Topics <- joined_oR %>% 
  select(sessionID, main.topic) %>% 
  arrange(sessionID)

topicPairs <- Matrix::crossprod(xtabs(~ sessionID + main.topic, data=Topics, sparse=TRUE))
dim(topicPairs)
# Sind tatsächlich nur 315 Main Topics in transactions vorhanden?
transactions_tbl %>% left_join(oR_tbl, by = "itemID") %>% select(main.topic) %>% unique() %>% summarise(n = n())
# ja - eigentlich 316, inkl. 1 NA
# Somit sind 315/360 = 87,5% der Main Topics in transactions vorhanden

# Test:
head(sort(topicPairs["AFF", ], decreasing = TRUE), 10)

## Author Pairs

Authors <- joined_oR %>% 
  select(sessionID, author) %>% 
  arrange(sessionID)

authorPairs <- Matrix::crossprod(xtabs(~ sessionID + author, data=Authors, sparse=TRUE))
dim(authorPairs)

# Sind tatsächlich nur 10655 Autoren in transactions vorhanden?
transactions_tbl %>% left_join(oR_tbl, by = "itemID") %>% select(author) %>% unique() %>% summarise(n = n())
# ja. Somit sind 10655/35727 = 29,82% der Autoren in transactions vorhanden

# Test:
head(sort(authorPairs["J. R. R. Tolkien", ], decreasing = TRUE), 10)

## Item Pairs

Items <- joined_oR %>% 
  select(sessionID, itemID) %>% 
  arrange(sessionID)

itemPairs <- Matrix::crossprod(xtabs(~ sessionID + itemID, data=Items, sparse=TRUE))
dim(itemPairs)

# Sind tatsächlich nur 24909 Items in transactions vorhanden?
transactions_tbl %>% left_join(oR_tbl, by = "itemID") %>% select(itemID) %>% unique() %>% summarise(n = n())
# ja. Somit sind 24909/78334 = 31,8% der Items in transactions vorhanden

# Test:
head(sort(itemPairs["1", ], decreasing = TRUE), 10)
