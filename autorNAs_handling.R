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
author_bearb <- right_join(authorNAs, evaluation_tbl)
author_bearb <- drop_na(author_bearb)        # 62 rows tauchen in evaluation_DS auf
view(author_bearb) 
oR_tbl %>%  filter(itemID == 24686) #Achtung! kein author, publisher, 
#main.topic, subtopic. Das Buch ist in evaluation Datensatz drin.
#TITLE: Christmas Coloring Book For Kids: 50 Holiday Unique Designs For Girls And Boys Ages 4-8

#Inspect group_by Verlag
#Arena Verlag: Malbücher, Kreuzworträtsel, Alter 5
#Ars Edition: auch Alterempfehlung 5, von subtopics ablesbar
#Coppenrath F: Stikerbuch, bastelbuch, malbuch, auch hier sind sie Ersatzprodukte
#Yo Yo Books: gleiche main.topic --> also kann man auch Author durch Verlag ersetzen 
#######################################################
######################################################
view(authorNAs) #group by publisher

#######################################################
#######################################################

#inspect publisher Albert Whitman & Company
view(authorNAs %>% filter(publisher== "Albert Whitman & Company")) #97 NAs
view(oR_tbl %>% filter(publisher== "Albert Whitman & Company")) #173 Books from this publisher
view(oR_tbl %>% filter(author == "Mike Litwin"))         #1 Book, same publisher
view(oR_tbl %>% filter(author == "Linda Joy Singleton")) #2 Books, same publisher
view(oR_tbl %>% filter(author == "Leslie Kimmelman"))    #3 Books, same publisher
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
#update
as_tibble(oR_tbl)
authorNAs <- oR_tbl %>% filter(author == "")
dim(authorNAs)
author_bearb <- right_join(authorNAs, evaluation_tbl)
author_bearb <- drop_na(author_bearb)        # 62 rows tauchen in evaluation_DS auf
view(author_bearb) #42 items
view(filter(oR_tbl, publisher=="Moewig"))
#Yo Yo Books # Moewing
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Yo Yo Books" & author == "",
                                    "Yo Yo Books" , author))
oR_tbl <- transform(oR_tbl, 
                    author = ifelse(publisher == "Moewig" & author == "",
                                    "Moewig" , author))
