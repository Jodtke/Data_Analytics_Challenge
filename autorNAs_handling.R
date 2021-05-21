############ Data Mining Cup ##########

### feature engineering
##autor NAs handling
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items2.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
dim(openRefine) #78334 X 6
evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
dim(evaluation)

## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions_raw)
dim(transactions_tbl)
oR_tbl <- as_tibble(openRefine)
dim(oR_tbl)
evaluation_tbl <- as_tibble(evaluation)

##check how many items with NAs in items_5 are in evaluation dataset
authorNAs <- oR_tbl %>% filter(author == "")
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
        author = ifelse(publisher == "Albert Whitman & Company" & author == "", "Albert Whitman" , author))

#replace Dorling Kindersley Ltd Dorling Kindersley with  Dorling Kindersley Verlag
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Dorling")))
oR_tbl <- transform(oR_tbl, 
                    publisher = ifelse(publisher == "Dorling Kindersley" & publisher == "Dorling Kindersley Ltd",
                                       "Dorling Kindersley Verlag" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Dorling Kindersley Verlag" & author == "",
                                    "Dorling Kindersley" , author))
#replace Trötsch with Trötsch Verlag Gmbh
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Trötsch")))
oR_tbl <- transform(oR_tbl, 
                    publisher = ifelse(publisher == "Trötsch", "Trötsch Verlag Gmbh", publisher))
view(oR_tbl %>% filter(publisher=="Trötsch Verlag Gmbh"))
#replace author for Publischer Trötsch Verlag Gmbh with Trötsch
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Trötsch Verlag Gmbh" & author == "", "Trötsch" , author))
count(oR_tbl %>% filter(author == "Trötsch")) #106 replacements

#Arena Verlag Gmbh and Arena
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Arena")))
oR_tbl <- transform(oR_tbl, 
                    publisher = ifelse(publisher == "Arena",
                                       "Arena Verlag Gmbh" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Arena Verlag Gmbh" & author == "",
                                    "Arena" , author))
view(filter(oR_tbl, author=="Arena")) #58 replacements
#Ars Edition Gmbh and Ars Edition
view(oR_tbl %>% filter(str_detect(oR_tbl$publisher, "^Ars")))
filter(oR_tbl, publisher=="Ars Vivendi") #only one in all data set
view(oR_tbl %>% filter(str_detect(oR_tbl$author, "^Annabell Stochay"))) # only one author for this unique publisher
oR_tbl <- transform(oR_tbl, 
                    publisher = ifelse(publisher == "Ars Edition",
                                       "Ars Edition Gmbh" , publisher))
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Ars Edition Gmbh" & author == "",
                                    "Ars Edition" , author))
#Yo Yo Books # Moewing
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Yo Yo Books" & author == "",
                                    "Yo Yo Books" , author))
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Moewig" & author == "",
                                    "Moewig" , author))

#update
as_tibble(oR_tbl)
authorNAs <- oR_tbl %>% filter(author == "")
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


#write_csv(oR_tbl, file="./Data/items3.csv", col_names=T)
