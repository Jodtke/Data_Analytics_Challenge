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
# Reihenfolge der Spalten verändern
joined_oR <- joined_oR[, c(7,1,8:10,2:6) ]
# Variablen rekodieren
joined_oR <- joined_oR %>%
  mutate(main.topic = as.factor(main.topic),
         itemID = as.character(itemID),
         sessionID = as.integer(sessionID))
head(joined_oR, n = 20)
# Transaction-DataFrame erstellen
transaction_frame <- joined_oR %>%
  group_by(sessionID) %>% # Variablen asugerichtet nach der SessionID
  arrange(sessionID) %>% # SessionID chronologisch von 0 - ENDE geordnet
  # Summe der Clicks, Warenkörbe, Käufe, Anzahl der Items & zugehörige ItemID's zusammengefasst in Tibble speichern
  summarize(nItems = n(), nClicks = sum(click), nBaskets = sum(basket), nOrders = sum(order),
            items = paste(as.character(itemID), collapse = ","))
transaction_frame <- data.frame(lapply(X = transaction_frame, FUN = as.factor)) # als DataFrame umwandeln und Variablen als Factor rekodieren
head(transaction_frame, n = 20)
# als Text-Datei speichern und als 'Transaction-Class' einlesen
write.table(transaction_frame, "transactions.txt", sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
transactions_matrix <- read_baskets("transactions.txt", sep = ";", info = c("sessionID","nItems"))

## Split Sets
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
#size(itemMatrix)
#itemInfo(itemMatrix)

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


### Transaction Matrix erstellen
trans_seq <- joined_oR %>%
  group_by(sessionID) %>%
  summarize(
    SIZE = n(),
    itemID = as.character(itemID),
    nOrder = sum(order),
    nClick = sum(click)
  )    
trans_seq



transaction_1 <- joined_oR %>%
  mutate(click = as.factor(click),
         basket = as.factor(basket),
         order = as.factor(order),
         title = as.factor(title),
         author = as.factor(author),
         publisher = as.factor(publisher),
         subtopics = as.factor(subtopics)) %>%
  as("transactions")
arules::summary(transaction_1)

transaction_2 <- as(joined_oR, "transactions")
rules2 <- apriori(transaction_2, parameter = list(minlen=2, support=0.005, confidence=0.8))
arules::summary(rules2)
inspect(head(sort(rules2, by="lift"), n = 10))




