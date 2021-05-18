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

############ Daten #############
joined_item_trans <- read_csv(file = "./Data/joined_item_trans.csv", col_names = T, col_types = cols(
  itemID=col_factor(),
  title=col_character(),
  author=col_character(),
  publisher=col_character(),
  main.topic=col_character(),
  subtopics=col_character(),
  sessionID=col_factor(),
  click=col_integer(),
  basket=col_integer(),
  order=col_integer()
))
head(joined_item_trans, n=10)
glimpse(joined_item_trans)
## Reihenfolge der Spalten verändern
joined_item_trans <- joined_item_trans[, c(7,1,8:10,2:6) ]
head(joined_item_trans, n=10)

## durchschnittliche Click-Order-Ratio pro Item berechnen
joined_item_trans$click_order_ratio <- joined_item_trans$click / joined_oR$order
joined_item_trans$click_order_ratio <- sapply(joined_item_trans$click_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
## durchschnittliche Basket-Order-Ration pro Item berechnen
joined_item_trans$basket_order_ratio <- joined_item_trans$basket / joined_item_trans$order
joined_item_trans$basket_order_ratio <- sapply(joined_item_trans$basket_order_ratio, function(x) ifelse(x=="Inf" | x=="NaN", 0, x))
head(joined_item_trans, n=10)
glimpse(joined_item_trans, n=10)

## Transaction-DataFrame erstellen
orders_data <- joined_item_trans %>%
  group_by(sessionID) %>% # Variablen asugerichtet nach der SessionID
  arrange(sessionID) %>% # SessionID chronologisch von 0 - ENDE geordnet
  filter(order > 0) %>% # nur wenn min. 1 Kauf geschehen ist
  # Summe der Clicks, Warenkörbe, Käufe, Anzahl der Items & zugehörige ItemID's zusammengefasst in Tibble speichern
  summarize(nOrderedItems = n(), nOrders = sum(order),
            items = paste0(as.character(itemID), collapse = ","),
            authors = paste0(as.character(author), collapse = ","),
            mainTopics = paste0(as.character(main.topic), collapse = ",")) %>%
  lapply(FUN = as.factor) %>% # alle Variablen als Faktor rekodieren & als Liste umwandeln
  data.frame() # Datenstruktur in DF umwandeln
head(orders_data, n = 20)

## als Text-Datei speichern und als 'Transaction-Class' einlesen
write.table(orders_data, "orders.txt", sep=";", row.names=FALSE, col.names=FALSE, quote=FALSE)
orders_matrix <- read_baskets("orders.txt", sep=";", info = c("sessionID","nOrderedItems", "nOrders", "items", "authors", "mainTopics"))


########## ROLF: Split Sets & ItemMatrix#########

## Beobachtungen extrahieren
itemMat_raw <- joined_item_trans %>%
  select(sessionID, itemID, order, author) %>% # nur drei essentielle Variablen: Session, Item & Kaufentscheidung
  head(n=36500) %>% # erste 36500 Beobachtungen
  spread(key = itemID, value = order)

## Zelleneinträge binär kodieren & Item-Matrix entwerfen
itemMat_raw <- lapply(itemMat_raw, function(x) ifelse(is.na(x) | x==0, F, T)) # einzelne Beobachtungen in 0 & 1 umwandeln
itemMat_raw <- as_tibble(itemMat_raw) # als Tibble konvertieren
itemMat_raw <- itemMat_raw[,-1] # sessionID als Variable entfernen
itemMat_raw <- as.matrix(itemMat_raw) # als Matrix umwandeln
itemMatrix <- as(itemMat_raw, "itemMatrix")
summary(itemMatrix)

## Rules mit apriori definieren
rules <- apriori(itemMatrix, parameter =  list(support = 0.00005, maxlen = 2, confidence = 0.5))

## Rules untersuchen, grafisch & manuell
summary(rules)
plot(rules, method = "graph")
inspect(head(sort(rules, by = "support"), n=10))
# hochfrequente Items suchen
oR_tbl %>% filter(itemID == 14093 | itemID == 47221)
oR_tbl %>% filter(itemID == 69073 | itemID == 27041)
oR_tbl %>% filter(itemID == 4626 | itemID == 61335)


