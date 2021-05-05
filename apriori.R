############ Data Mining Cup ##########

### Apriori

## Packages
library(tidyverse)
#library(janitor)
#library(summarytools)
#library(tmaptools)
library(arules)
library(arulesViz)
library(arulesSequences)

## Arbeitsverzeichnis
getwd()

## Daten
#items_raw <- read.csv(file = "./Data/items.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
#dim(items_raw)    #78 334 x 6
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/oR_items.csv", header = T, sep = ";", row.names = NULL, stringsAsFactors = F, encoding = "UTF8-8")
dim(openRefine)

## als Tibble kovertieren
#items_tbl <- as_tibble(items_raw)
# glimpse(items_tbl)
# head(items_tbl, n = 20)
transactions_tbl <- as_tibble(transactions_raw)
glimpse(transactions_tbl)
head(transactions_tbl, n = 10)
# bereinigter Datensatz aus Open Refine
oR_tbl <- as_tibble(openRefine)
glimpse(oR_tbl)
head(oR_tbl, n = 10)

## Joining
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
glimpse(joined_oR)
head(joined_oR, n = 20)
## Reihenfolge der Spalten verändern
joined_oR <- joined_oR[c(1,7:10,2:6)] 
joined_oR <- joined_oR %>%
  mutate(main.topic = as.factor(main.topic),
         itemID = as.character(itemID),
         sessionID = as.character(sessionID))
head(joined_oR, n = 20)

## Split Sets
# Beobachtungen extrahieren
itemMat_raw <- joined_oR %>%
  select(sessionID, itemID, order) %>% # nur drei essentielle Variablen: Session, Item & Kaufentscheidung
  head(n=10000) %>% # erste 10000 Beobachtungen
  spread(key = itemID, value = order)
# Zelleneinträge binär kodieren & Item-Matrix entwerfen
itemMat_raw <- lapply(itemMat_raw, function(x) ifelse(is.na(x) | x==0, 0, 1)) # einzelne Beobachtungen in 0 & 1 umwandeln
itemMat_raw <- as_tibble(itemMat_raw) # als Tibble konvertieren
itemMat_raw <- itemMat_raw[,-1] # sessionID als Variable entfernen
itemMat_raw <- as.matrix(itemMat_raw) # als Matrix umwandeln
itemMatrix <- as(itemMat_raw, "itemMatrix")
arules::summary(itemMatrix)
#size(itemMatrix)
#itemInfo(itemMatrix)

# Rules mit apriori definieren
rules <- apriori(itemMatrix, parameter =  list(support = 0.0005, confidence = 0.5))
# Rules untersuchen, grafisch & "normal"
summary(rules)
plot(rules, method = "graph")
inspect(head(sort(rules, by = "lift"), n=10))








