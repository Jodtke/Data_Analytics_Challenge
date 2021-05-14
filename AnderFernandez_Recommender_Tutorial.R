############ Data Mining Cup ##########

### Apriori

## Packages
library(tidyverse)
#library(janitor)
library(arules)
library(arulesViz)
library(arulesSequences)

## Arbeitsverzeichnis
getwd()

## Daten
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items_bearbeitet4.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
dim(openRefine)

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

## Variablen rekodieren in Factor
joined_oR <- as_tibble(lapply(joined_oR, factor))
head(joined_oR, n=20)
# an 'numerische' Factors den Zusatz 'ID' hängen, damit später keine Probleme auftreten
joined_oR$sessionID <- as.factor(paste0("SID.", joined_oR$sessionID))
joined_oR$itemID <- as.factor(paste0("IID.", joined_oR$itemID))
head(joined_oR, n=20)

# NA-Daten von click, basket und order rasuwerfen
joined_tbl_ohneNAs <- joined_oR %>%
  filter(!is.na(click) & !is.na(basket) & !is.na(order))
head(joined_tbl_ohneNAs, n=20) # ca 50.000 Zeilen entfernt & etwa 365.000 Zeilen verbleiben


  


