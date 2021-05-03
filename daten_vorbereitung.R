############ Data Mining Cup ##########

### Vorbereitung

## Packages
library(tidyverse)
library(summarytools)


## Arbeitsverzeichnis
getwd()

## Daten
items_raw <- read.csv(file = "./Data/items.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(items_raw)    #78 334 x 6
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5

# als Tibble kovertieren
items_tbl <- as_tibble(items_raw)
transactions_tbl <- as_tibble(transactions_raw)

###### Datenexploration ######
# Items
glimpse(items_tbl)
head(items_tbl, n = 20)
items_tbl %>% 
  select(itemID, title, author, publisher) %>%
  group_by(title) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ####### nach title
items_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ###### nach author
items_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(publisher) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ###### nach publisher
items_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author, title) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ##### nach author & title
items_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author, publisher) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ##### nach author & publisher

# Transactions
glimpse(transactions_tbl)
head(transactions_tbl, n = 20)
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nClick))
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nBasket))
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nOrder))
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(N))

## summarytools
# Items
summarytools::dfSummary(items_tbl)
summarytools::freq(items_tbl$author)
# summarytools::freq(items_tbl) # ACHTUNG: Häufigkeit für JEDE unterschiedlich Beobachtung

# Transactions
summarytools::freq(transactions_tbl$click)
summarytools::freq(transactions_tbl$basket)
summarytools::freq(transactions_tbl$order)
summarytools::view(descr(transactions_tbl))

#Merge Datasets mit alle sessionIDs
length(unique(items_tbl$itemID))
length(unique(transactions_tbl$itemID))
joined_tbl <- left_join(items_tbl,transactions_tbl, by = "itemID") #418 568 x 10

#reifolge der Spalten verändern
joined_tbl <- joined_tbl[c(1,7:10,2:6)] 
joined_tbl <- joined_tbl %>% mutate(main.topic = as.factor(main.topic))
joined_tbl

#wie oft jeder main.topic im Laden vorkommt
count_maintopics <- count(items_tbl, main.topic) %>% arrange(desc(n))
count_maintopics
ggplot(count_maintopics) + geom_histogram(aes(x=n))
count_maintopics %>% filter(n > 500)   #33 Themen am meinsten vorhanden
ggplot(count_maintopics) + geom_histogram(aes(x=n), breaks = seq(0,500,25))
ggplot(count_maintopics) + geom_histogram(aes(x=n), breaks = seq(0,100,25))
count_maintopics %>% filter(n < 25)    #540/700 Themen kommen ganz selten vor
#dementsprechend müssen sie gruppiert werden auf der Phase feature engineering


########################
#Top Themen im Laden 

#FM Fantasyliteratur
#YFB Kinder/Jugendliche: Gegenwartsliteratur
#FL Science-Fiction
#YFH Kinder/Jugendliche: Fantasy
#YFC Kinder/Jugendliche: Action- und Abenteuergeschichten


#main_topics,an die Kunden am meisten Interesse haben
joined_tbl %>% group_by(main.topic) %>% count(main.topic) %>% arrange(desc(n))

########################
#Top Themen an die Kunden Interesse haben 

#FMB  Fantasy
#YFH  Fantasy and magical realism (Children’s/Teenage)
#YBG Interactive and activity books and packs…
#FM Fantasyliteratur
#YFHR Fantasy romance (Teenage)

length(unique(transactions_tbl$sessionID)) # unique sessions 271 983 
##nur duplicated sessions
transactions_tbl[transactions_tbl$sessionID %in% 
<<<<<<< HEAD
                   transactions_tbl$sessionID[duplicated(transactions_tbl$sessionID)],]
=======
               transactions_tbl$sessionID[duplicated(transactions_tbl$sessionID)],]
>>>>>>> cc28ea4e1b309f312a7bee5b1704ec313d29fe87
