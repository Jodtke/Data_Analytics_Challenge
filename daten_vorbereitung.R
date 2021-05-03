############ Data Mining Cup ##########

############ Data Analytics Challenge ##########

### Vorbereitung

## Packages
library(tidyverse)
library(summarytools)


## Arbeitsverzeichnis
setwd("/Volumes/Secomba/ejahnke/Boxcryptor/iCloud/Uni/KU Ingolstadt/SoSe 21/Data Analytics (Challenge)/DMC-2021-Task/")
getwd()

## Daten
items_raw <- read.csv(file = "items.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
transactions_raw <- read.csv(file = "transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)

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


