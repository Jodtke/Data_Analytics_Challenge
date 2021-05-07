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
# Reihenfolge der Spalten verändern
joined_oR <- joined_oR[, c(7,1,8:10,2:6) ]
# Variablen rekodieren
joined_oR <- joined_oR %>%
  mutate(main.topic = as.factor(main.topic),
         itemID = as.character(itemID),
         sessionID = as.integer(sessionID))
head(joined_oR, n = 20)
# Transaction-DataFrame erstellen
orders_data <- joined_oR %>%
  group_by(sessionID) %>% # Variablen asugerichtet nach der SessionID
  arrange(sessionID) %>% # SessionID chronologisch von 0 - ENDE geordnet
  filter(order > 0) %>% # nur wenn min. 1 Kauf geschehen ist
  # Summe der Clicks, Warenkörbe, Käufe, Anzahl der Items & zugehörige ItemID's zusammengefasst in Tibble speichern
  summarize(nOrderedItems = n(), nOrders = sum(order),
            items = paste(as.character(itemID), collapse = ",")) %>%
  lapply(FUN = as.factor) %>% # alle Variablen als Faktor rekodieren & als Liste umwandeln
  data.frame() # Datenstruktur in DF umwandeln
head(orders_data, n = 20)
# als Text-Datei speichern und als 'Transaction-Class' einlesen
write.table(orders_data, "orders.txt", sep=";", row.names=FALSE, col.names=FALSE, quote=FALSE)
orders_matrix <- read_baskets("orders.txt", sep=";", info = c("sessionID","nItems"))
# # cspade anwenden (SPADE = "Sequential Pattern Discovery using Equivalence classes")
# spade1 <- cspade(orders_matrix, parameter = list(support = 0.3), control = list(verbose = TRUE))

## ROLF: Split Sets
# Beobachtungen extrahieren
itemMat_raw <- transactions_tbl%>%
  select(sessionID, itemID, order) %>% # nur drei essentielle Variablen: Session, Item & Kaufentscheidung
  head(n=36500) %>% # erste 36500 Beobachtungen
  spread(key = itemID, value = order)
# Zelleneinträge binär kodieren & Item-Matrix entwerfen
itemMat_raw <- lapply(itemMat_raw, function(x) ifelse(is.na(x) | x==0, F, T)) # einzelne Beobachtungen in 0 & 1 umwandeln
itemMat_raw <- as_tibble(itemMat_raw) # als Tibble konvertieren
itemMat_raw <- itemMat_raw[,-1] # sessionID als Variable entfernen
itemMat_raw <- as.matrix(itemMat_raw) # als Matrix umwandeln
itemMatrix <- as(itemMat_raw, "itemMatrix")
summary(itemMatrix)
# Rules mit apriori definieren
rules <- apriori(itemMatrix, parameter =  list(support = 0.00005, maxlen = 9, confidence = 0.5))
# Rules untersuchen, grafisch & "normal"
summary(rules)
#plot(rules, method = "graph")
inspect(head(sort(rules, by = "support"), n=10))
# hochfrequente Items suchen
oR_tbl %>% filter(itemID == 14093 | itemID == 47221)
oR_tbl %>% filter(itemID == 69073 | itemID == 27041)
oR_tbl %>% filter(itemID == 4626 | itemID == 61335)


### TEST: Transaction Matrix erste
transaction_2 <- as(joined_oR, "transactions")
rules2 <- apriori(transaction_2, parameter = list(minlen=2, support=0.005, confidence=0.8))
arules::summary(rules2)
inspect(head(sort(rules2, by="lift"), n = 10))




