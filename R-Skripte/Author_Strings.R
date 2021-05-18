######### Main Topics auf 3 Buchstaben kürzen ############

# libraries
library(tidyverse)
library(janitor)

# daten
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items_bearbeitet4.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF8-8")
dim(openRefine)
transactions_tbl <- as_tibble(transactions_raw)
oR_tbl <- as_tibble(openRefine)
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
rm(openRefine, oR_tbl, transactions_raw, transactions_tbl)

# einzelne characters pro string zählen mit 'nchar' (base R)
nchar(joined_oR$main.topic[[1]]) # 3 characters

# main topics filter mit größer/gleich 4 characters
joined_oR$main.topic %>% function(x) ifelse(nchar(x)==4, str_trunc(x, width=3, side = "right"), x)
# main topics als variable exkludieren, der schnelligkeit wegen
main_topic <- joined_oR$main.topic


