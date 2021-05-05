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

# als Tibble kovertieren
items_tbl <- as_tibble(items_raw)
# glimpse(items_tbl)
# head(items_tbl, n = 20)
transactions_tbl <- as_tibble(transactions_raw)
glimpse(transactions_tbl)
head(transactions_tbl, n = 10)
# bereinigter Datensatz aus Open Refine
oR_tbl <- as_tibble(openRefine)
glimpse(oR_tbl)
head(oR_tbl, n = 10)
