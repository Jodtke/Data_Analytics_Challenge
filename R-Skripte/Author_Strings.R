######### Main Topics auf 3 Buchstaben kürzen ############

# libraries
library(tidyverse)
library(janitor)

# daten
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items_bearbeitet5.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
dim(openRefine)
transactions_tbl <- as_tibble(transactions_raw)
oR_tbl <- as_tibble(openRefine)
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
head(joined_oR, n=10)
rm(openRefine, oR_tbl, transactions_raw, transactions_tbl)

# einzelne characters pro string zählen mit 'nchar' (base R)
nchar(joined_oR$main.topic[[1]]) # 3 characters
joined_oR$main.topic[nchar(joined_oR$main.topic) > 3] # zum Beispiel: WZSN
joined_oR$main.topic[joined_oR$main.topic == "WZSN"] # ziemlich viele Fälle!

# Trimmen der Main Topics auf max. 3 Stellen mit str_trunc des strinR Package aus tidyverse & sapply()
joined_oR <- joined_oR %>%
  mutate(
    main.topic = sapply(main.topic, function(x) if_else(nchar(x) > 3, str_trunc(x, width=3), x)),
    main.topic = as.character(main.topic)
  )

# Überprüfen
joined_oR %>% select(main.topic) %>% filter(nchar(main.topic) > 3) # 0 Fälle von MainTopic mit 4 characters pro String
joined_oR %>% select(main.topic) %>% filter(main.topic == "WZSN") # 0 Fälle von WZSN in MainTopic
joined_oR %>% select(main.topic) %>% filter(main.topic == "WZS") # viele Fälle von WZS --> Check!

# alternativ: mit Trimmer-Funktion --> Funktioniert noch nicht ganz? ):
mainTopic_trimmer <- function(x) {
  for (idx in 1:length(x)) {
    if (nchar(idx) > 3) {
      str_trunc(idx, width=3)
    } else {
      idx = idx
    }
  }
}



