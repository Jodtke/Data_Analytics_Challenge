############ Data Mining Cup ##########

###
##Rules with sessionID
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
#dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items3.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
#dim(openRefine) #78334 X 6
evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
#dim(evaluation)

## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions_raw)
#dim(transactions_tbl)
oR_tbl <- as_tibble(openRefine)
#dim(oR_tbl)
evaluation_tbl <- as_tibble(evaluation)

#join the files
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
#Reifolge der Spalten verändern 
joined_oR <- select(joined_oR, c(7:10,1,2,5,3,4,6))
#abspeichern nur die sessions, die >= 2 Bücher hatten
dup.ses <- joined_oR[joined_oR$sessionID %in%
                   joined_oR$sessionID[duplicated(joined_oR$sessionID)],]  # das sind knapp 183k sessions
dup.ses
#view(dup.ses)
length(unique(dup.ses$itemID)) #für 67896 Bücher würde das funktionieren

##prüfen wie viel Bücher von denen, die in transaction DS drin sind, auch in evaluation DS erscheinen.
#Ob das Sinn macht der Algorithmus zu basteln und der Ansatz zu implimentieren 
pruff_ses <- right_join(dup.ses, evaluation_tbl, by = "itemID")
#view(pruff_ses %>% group_by(itemID))
length(unique(pruff_ses$itemID))
pruff_ses <- drop_na(pruff_ses)  
pruff_ses %>% group_by(itemID) %>% summarise(n=n()) # 304 Bücher in DS evaluation
##############################################

#z.b. wir suchen Recomendation für item_5468
item <- 5468
this_ses <- dup.ses %>% group_by(sessionID) %>%    #aus dem Datensatz, wo nur dupplicated sessions drin
            filter(itemID==item) %>% select(sessionID)
this_ses <- as_vector(this_ses)
this_books <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID)
this_books
#this_books <- this_books %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
              summarise(n=n()) %>% arrange(desc(n)) %>% filter(n == max(n))


this_books <- this_books %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
  summarise(n=n()) %>% arrange(desc(n))
neib <- this_books$sessionID
view(neib)
neid <- dup.ses %>% filter(sessionID%in%neib)
view(neid)
neid %>% group_by(itemID) %>% summarise(nClick=sum(click),nBasket=sum(basket),norder=sum(order)) %>% view()
this_books
recommended_session <- this_books[[1,1]]
list_of_recommendations <- dup.ses %>% filter(sessionID==recommended_session)
View(list_of_recommendations)

#Recommendation Function based on sessionID
active_books <- c()
Recommendation_function <- function(){
  
}