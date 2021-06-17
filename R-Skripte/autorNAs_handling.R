############ Data Mining Cup ##########

### feature engineering
##autor NAs handling
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
transactions <- read_delim(file="./Data/transactions.csv", col_names=T, delim="|", col_types=cols(
  sessionID = col_factor(),
  itemID = col_factor(),
  click = col_integer(),
  basket = col_integer(),
  order = col_integer()
))
dim(transactions)  #365 143 x 5

items <- read_csv(file = "./Data/items2.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_character(),
  author=col_character(),
  publisher=col_character(),
  `main topic`=col_character(),
  subtopics=col_character()
))
items <- items %>%
  rename(main.topic = `main topic`)
head(items, n=20)

evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
evaluation$itemID <- as.factor(evaluation$itemID)

## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions)
dim(transactions_tbl)
oR_tbl <- items
dim(oR_tbl)
evaluation_tbl <- as_tibble(evaluation)

##
save1 <- oR_tbl %>% filter(is.na(author)) %>% group_by(publisher) %>% summarise(n=n())
oR_tbl %>% filter(publisher == "Ars Edition Gmbh") %>% filter(is.na(author))
save1 <- as.numeric(save1$n)
hist(save1)

##check how many items with NAs in items_5 are in evaluation dataset
authorNAs <- oR_tbl %>% filter(is.na(author)) %>% group_by(publisher) %>% summarise(n=n())
head(authorNAs, n=20)
dim(authorNAs)
view(authorNAs)
author_bearb <- right_join(authorNAs, evaluation_tbl)
author_bearb <- drop_na(author_bearb)        # 62 rows tauchen in evaluation_DS auf
count(oR_tbl, main.topic) %>% arrange(desc(n))
#Inspect group_by Verlag
#Arena Verlag: Malbücher, Kreuzworträtsel, Alter 5
#Ars Edition: auch Alterempfehlung 5, von subtopics ablesbar
#Coppenrath F: Stikerbuch, bastelbuch, malbuch, auch hier sind sie Ersatzprodukte
#Yo Yo Books: gleiche main.topic --> also kann man auch Author durch Verlag ersetzen 
view(authorNAs) #group by publisher

#######################################################
#######################################################

#replace NAs with Albert Whitman
oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Albert"))
oR_tbl <- transform(oR_tbl, 
        author = if_else(publisher == "Albert Whitman & Company" & is.na(author), "Albert Whitman" , author))

#replace Dorling Kindersley Ltd Dorling Kindersley with  Dorling Kindersley Verlag
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Dorling")))
oR_tbl <- transform(oR_tbl, 
                    publisher = if_else(publisher == "Dorling Kindersley" & publisher == "Dorling Kindersley Ltd",
                                       "Dorling Kindersley Verlag" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Dorling Kindersley Verlag" & is.na(author),
                                    "Dorling Kindersley" , author))
#replace Trötsch with Trötsch Verlag Gmbh
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Trötsch")))
oR_tbl <- transform(oR_tbl, 
                    publisher = if_else(publisher == "Trötsch", "Trötsch Verlag Gmbh", publisher))
view(oR_tbl %>% filter(publisher=="Trötsch Verlag Gmbh"))
#replace author for Publischer Trötsch Verlag Gmbh with Trötsch
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Trötsch Verlag Gmbh" & is.na(author), "Trötsch" , author))
count(oR_tbl %>% filter(author == "Trötsch")) #106 replacements

#Arena Verlag Gmbh and Arena
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Arena")))
oR_tbl <- transform(oR_tbl, 
                    publisher = if_else(publisher == "Arena",
                                       "Arena Verlag Gmbh" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Arena Verlag Gmbh" & is.na(author),
                                    "Arena" , author))
view(filter(oR_tbl, author=="Arena")) #58 replacements
#Ars Edition Gmbh and Ars Edition
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Ars")))
filter(oR_tbl, publisher=="Ars Vivendi") #only one in all data set
view(oR_tbl %>% filter(str_detect(oR_tbl$author, "^Annabell Stochay"))) # only one author for this unique publisher
oR_tbl <- transform(oR_tbl, 
                    publisher = if_else(publisher == "Ars Edition",
                                       "Ars Edition Gmbh" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Ars Edition Gmbh" & is.na(author),
                                    "Ars Edition" , author))
#Yo Yo Books # Moewing
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Yo Yo Books" & is.na(author),
                                    "Yo Yo Books" , author))
oR_tbl <- transform(oR_tbl, 
                    author = if_else(publisher == "Moewig" & is.na(author),
                                    "Moewig" , author))

#update
#as_tibble(oR_tbl)
authorNAs <- oR_tbl %>% filter(is.na(author))
dim(authorNAs)
view(authorNAs)
#handeln 

#Applewood + Applewood Books
#Baen
#Blackharepress
#Bob Jones Univ Pr
#Bonnier Books Ltd UK
#Books On Demand
#Brianne Mitchell
#Butzon & Bercker
#Carlsen + Carlsen Verlag Gmbh
#Castalia House
#Childs Play + Child's Play International Ltd
#Christophorus Verlag
#Coppenrath and Coppenrath F
#Dörfler + Dörfler Verlag Gmbh
#Edition Michael Fischer
#Fischer Kjb
#Loewe Verlag Gmbh
#St. Benno
#Truant Ug
#Panini Verlags Gmbh
#White Star Verlag + White Star
#Usborne Verlag + Usborne Publishing Ltd
#Ullmann Medien+ Ullmann Medien Gmbh
#Uhrwerk Verlag+Uhrwerk
#Tessloff Verlag+Tessloff Medienvertrieb+Tessloff
#Sweet Cherry Publishing
#Stone Arch Books
#Steck Vaughn Co + Star Trek
#Simon Spotlight
#Simon Pulse
#Simon + Schuster Inc. + Simon & Schuster Books For Young Readers +Simon & Schuster
#Schwager Und Steinlein
#Schuenemann C.E.
#Scholastic Ltd.+Scholastic
#Schmid F. X.
#Saint Philip Street Press


# write_csv(oR_tbl, file="./Data/items3.csv", col_names=T)

############### items4 bearbeiten --> Authoren NA's ersetzen durch Publisher & evtl. Publisher durch Authoren ersetzen #############
# items4 <- read_csv(file="./Data/items4.csv", col_names=T, col_types=cols(
#   itemID=col_factor(),
#   title=col_factor(),
#   author=col_character(),
#   publisher=col_character(),
#   main.topic=col_factor(),
#   subtopics=col_character()
# ))
# head(items4, n=20)
# glimpse(items4)

items4 <- oR_tbl #ALTERNATIV, WENN ITEMS 4 NICHT MEHR EXISTIERT!!
glimpse(items4)
str(items4)
head(items4, n=20)

######## Authoren NAs ##########
view(items4 %>% filter(is.na(author)))
## Authoren NA's durch jeweilige angegebene Publisher zu ersetzen
items4 <- items4 %>%
  mutate(
    author = if_else(is.na(author), publisher, author)
  )
view(items4 %>% filter(is.na(author))) # keine NAs in den Autoren

########### Publisher NAs #############
view(items4 %>% filter(is.na(publisher)))
## publisher NA#s durch jeweilige Authoren ersetzen
items4 <- items4 %>%
  mutate(
    publisher = ifelse(is.na(publisher), author, publisher)
  )
view(items4 %>% filter(is.na(publisher))) # keine NAs in den Publishern

## neue csv items5 schreiben
write_csv(items4, file="./Data/items5.csv", col_names=T)



