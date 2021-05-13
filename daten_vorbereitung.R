############ Data Mining Cup ##########

### Vorbereitung

## Packages
library(tidyverse)
#library(janitor)
#library(summarytools)
#library(tmaptools)
library(arules)
library(arulesViz)

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
# items_tbl <- as_tibble(items_raw)
# glimpse(items_tbl)
# head(items_tbl, n = 20)
transactions_tbl <- as_tibble(transactions_raw)
glimpse(transactions_tbl)
head(transactions_tbl, n = 10)
# bereinigter Datensatz aus Open Refine
oR_tbl <- as_tibble(openRefine)
glimpse(oR_tbl)
head(oR_tbl, n = 10)

################################## Datenexploration ###########################################
### Items
## Häufigste Buchtitel im Handel
# items_tbl %>% 
#   select(itemID, title, author, publisher) %>%
#   group_by(title) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))
# ## Häufigste Autoren im Handel
# items_tbl %>%
#   select(itemID, title, author, publisher) %>%
#   group_by(author) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))
# ## Häufigste Verlage im Handel
# items_tbl %>%
#   select(itemID, title, author, publisher) %>%
#   group_by(publisher) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))
# ## Häufigste Kombination aus Autor & Buchtitel
# items_tbl %>%
#   select(itemID, title, author, publisher) %>%
#   group_by(author, title) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))
# ## Häufigste Kombination aus Autor & Verlag
# items_tbl %>%
#   select(itemID, title, author, publisher) %>%
#   group_by(author, publisher) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))

### Items (open refine)
## Häufigste Buchtitel im Handel
oR_tbl %>% 
  select(itemID, title, author, publisher) %>%
  group_by(title) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
## Häufigste Autoren im Handel
oR_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
## Häufigste Verlage im Handel
oR_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(publisher) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
## Häufigste Kombination aus Autor & Buchtitel
oR_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author, title) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
## Häufigste Kombination aus Autor & Verlag
oR_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author, publisher) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

### Transactions
## Häufigste Clicks einer itemID
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nClick))
## Häufigste Warenkörbe einer itemID
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nBasket))
## Häufigste Käufe einer itemID
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(nOrder))
## am Häufigsten auftretende itemID in SessionID (allgemein)
transactions_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click), nBasket = sum(basket), nOrder = sum(order), N = n()) %>%
  arrange(desc(N))

# summarytools::freq(transactions_tbl$click)
# summarytools::freq(transactions_tbl$basket)
# summarytools::freq(transactions_tbl$order)
#summarytools::view(descr(transactions_tbl))

# einzigartige itemID's
#length(unique(items_tbl$itemID))
length(unique(transactions_tbl$itemID))
length(unique(oR_tbl$itemID))

length(unique(transactions_tbl$sessionID)) # einzigartige sessions 271 983 
## nur doppelte sessions
transactions_tbl[transactions_tbl$sessionID %in%
                   transactions_tbl$sessionID[duplicated(transactions_tbl$sessionID)],]

#################################### Joining ###################################################
# #### items Datensatz ()
# joined_tbl <- left_join(items_tbl,transactions_tbl, by = "itemID") #418 568 x 10
# glimpse(joined_tbl)
# head(joined_tbl, n = 20)
# # Reihenfolge der Spalten verändern
# joined_tbl <- joined_tbl[c(1,7:10,2:6)] 
# joined_tbl <- joined_tbl %>% mutate(main.topic = as.factor(main.topic))
# head(joined_tbl, n = 20)
# # wie oft jeder main.topic im Laden vorkommt
# count_maintopics <- count(items_tbl, main.topic) %>% arrange(desc(n))
# head(count_maintopics, n = 20) 
### Bar plot with 10 main topics in the book shop
# g_by_topic <- items_tbl %>% 
#   select(itemID, main.topic) %>%
#   group_by(main.topic) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N)) ####### nach topic
# main_topics <- g_by_topic[1:10,]
# main_topics
# 
# #tmaptools::palette_explorer() 
# my_palette <- c("#61BB6D","#7CC77A","#C3E698", 
#                 "#DBF0A4", "#E9F6AF", "#F7FCBA",
#                 "#FBFDCF", "#FFFFE5", "#95D284",
#                 "#AEDD8E")
# main_topics <- main_topics %>%
#   mutate(lbls = c("FM"="Fantasy literature", 
#                   "YFB" = "Children’s and teenage: contemporary literature",
#                   "FL" = "Science-Fiction",
#                   "YFH" = "Children’s and teenage: fantasy and magical realism ",
#                   "YFC" = "Children’s and teenage: action and adventure",
#                   "YF" = "Children’s and teenage: fiction and true stories",
#                   "YBG" = "Learning material: interactive and activity books",
#                   "FMB" = "Fiction and related items",
#                   "YFCF" = "Children’s: crime and mystery fiction",
#                   "YFJ" = "Children’s: traditional stories"))
# # main_topics
# g <- ggplot(data = main_topics, mapping = aes(x = reorder(main.topic, desc(N)), y = N, fill = my_palette))
# g + geom_col(fill = my_palette) +
#   geom_text(data = main_topics, aes(x=main.topic, y=100, label = lbls, hjust=0, angle=90)) +
#   ggtitle("10 best Bookstore available topics") +
#   theme(plot.title = element_text(size = 25),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16)) +
#   xlab("Topic Entcoding acc. BIC(Book Industry Communications)") +
#   ylab("Frequency")
# jpeg(file = "10_available_topics.jpeg")
# ggsave("10_available_topics.jpeg", width = 297, height = 210, units = "mm")
# 
# ####HEATMAPE with 10 main best sell topics 
# 
# #joined_tbl %>%  select(itemID, main.topic, click, basket, order) %>%
# #group_by(main.topic)%>%
# bestseller_topics <- joined_tbl %>%
#   group_by(itemID) %>%
#   summarise(nClick = sum(click),
#             nBasket = sum(basket), 
#             nOrder = sum(order), 
#             N = n()) %>%
#   arrange(desc(nOrder)) %>%
#   print()


############################## Joining - Open Refine Datensatz ##################################
#### Open Refine Datensatz
joined_oR <- left_join(oR_tbl, transactions_tbl, by = "itemID")
glimpse(joined_oR)
head(joined_oR, n = 20)
# Reihenfolge der Spalten verändern
joined_oR <- joined_oR[c(1,7:10,2:6)] 
joined_oR <- joined_oR %>%
  mutate(main.topic = as.factor(main.topic),
         itemID = as.character(itemID),
         sessionID = as.character(sessionID))
head(joined_oR, n = 20)

# wie oft jeder main.topic im Laden vorkommt
count_maintopics <- count(joined_oR, main.topic) %>% arrange(desc(n))
head(count_maintopics, n = 20) 

# grafischer Überblick
ggplot(count_maintopics) + geom_histogram(aes(x=n))
# über 500 Vorkommen
count_maintopics %>% filter(n > 500)   #33 Themen am meinsten vorhanden
# grafischer Überblick
ggplot(count_maintopics) + geom_histogram(aes(x=n), breaks = seq(0,500,25))
ggplot(count_maintopics) + geom_histogram(aes(x=n), breaks = seq(0,100,25))
# Themen die weniger als 25 mal vorkommen
count_maintopics %>% filter(n < 25)    #540/700 Themen kommen ganz selten vor
#dementsprechend müssen sie gruppiert werden auf der Phase feature engineering

############################ Aufbereitung: Genres & Subgenres #################################
### Top-Themen im Laden 
# FM Fantasyliteratur
# YFB Kinder/Jugendliche: Gegenwartsliteratur
# FL Science-Fiction
# YFH Kinder/Jugendliche: Fantasy
# YFC Kinder/Jugendliche: Action- und Abenteuergeschichten

#main_topics,an die Kunden am meisten Interesse haben
joined_oR %>% group_by(main.topic) %>% count(main.topic) %>% arrange(desc(n))

### Top Themen an die Kunden Interesse haben 
# FMB  Fantasy
# YFH  Fantasy and magical realism (Children’s/Teenage)
# YBG Interactive and activity books and packs…
# FM Fantasyliteratur
# YFHR Fantasy romance (Teenage)

##################################### Visualisierung ##########################################
### Bar plot with 10 main topics in the book shop
g_by_topic <- oR_tbl %>% 
  select(itemID, main.topic) %>%
  group_by(main.topic) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ####### nach topic
main_topics <- g_by_topic[1:10,]
main_topics

#tmaptools::palette_explorer() 
my_palette <-  c("#61BB6D","#7CC77A","#95D284",
                 "#AEDD8E", "#C3E698", "#DBF0A4", "#E9F6AF", "#F7FCBA",
                 "#FBFDCF", "#FFFFE5")
main_topics <- main_topics %>% mutate(lbls = c("FM"="Fantasy literature", 
                                "YFB" = "Children’s and teenage: contemporary literature",
                                "FL" = "Science-Fiction",
                                "YFH" = "Children’s and teenage: fantasy and magical realism ",
                                "YFC" = "Children’s and teenage: action and adventure",
                                "YF" = "Children’s and teenage: fiction and true stories",
                                "YBG" = "Learning material: interactive and activity books",
                                "FMB" = "Fiction and related items",
                                "YFCF" = "Children’s: crime and mystery fiction",
                                "YFJ" = "Children’s: traditional stories"))
main_topics
#x = reorder(main.topic, desc(N))
#geom_text(data = main_topics, aes(x=main.topic, y=100, label = lbls, hjust=0, angle=90))+
g <- ggplot(data = main_topics, mapping = aes(y = reorder(main.topic, N), x = N, fill = my_palette))
g + geom_col(fill = my_palette) +
  geom_text(data = main_topics, aes(y=main.topic, x=100, label = lbls, hjust=0), size = 5)+
    ggtitle("10 best Bookstore available topics") +
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  ylab("Topic Entcoding acc. BIC(Book Industry Communications)") +
  xlab("Frequency")
#file = "10_available_topics.jpeg")
ggsave("10_available_topics.jpeg", width = 297, height = 210, units = "mm")

###############################################################################
###############################################################################
##bar plot bestseller topics in the shop####
#library(tmaptools)
bestseller_by_topics <- joined_tbl %>%
  group_by(main.topic) %>%
  summarise(nOrder = sum(order)) %>% 
  arrange(desc(nOrder)) %>% print()

bestseller_by_topics <- bestseller_by_topics[1:10,]
bestseller_by_topics
#tmaptools::palette_explorer() 
my_palette2 <-c("#FE9929","#FEBE4A","#FEAD3B","#FECF66","#FEDB81",
                "#FEE79B","#FEEFAC","#FFF7C0","#FFFBD1","#FFFFE5")
bestseller_by_topics <- bestseller_by_topics %>% mutate(lbls = c("JBSF1" = "Social groups and identities, gender studies",
                                                                 "QRSA" = "Ancient Egyptian religion and mythology",
                                                                 "WHJ" = "Jokes and riddles",
                                                                 "VXQM3" = "Monsters and legendary beings",
                                                                 "YNVD3" = "Hobbies, quizzes and games (Children’s/Teenage)",
                                                                 "YBFV5" = "Early learning material",
                                                                 "WFH" = "Handicrafts, decorative arts",
                                                                 "TCBG" = "Genetic engineering",
                                                                 "YBLM" = "Children’s: learning books",
                                                                 "YNPH1" = "Handicrafts(Children’s)"))
  
bestseller_by_topics
#x = reorder(main.topic, desc(N))
#geom_text(data = main_topics, aes(x=main.topic, y=100, label = lbls, hjust=0, angle=90))+
g <- ggplot(data = bestseller_by_topics, mapping = aes(y = reorder(main.topic, nOrder), x = nOrder, fill = my_palette2))
g + geom_col(fill = my_palette2) +
  geom_text(data = bestseller_by_topics, aes(y=main.topic, x=2, label = lbls, hjust=0), size = 5)+
  ggtitle("10 bestseller topics in the Bookstore") +
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  ylab("Topic Entcoding acc. BIC(Book Industry Communications)") +
  xlab("Frequency")
#(file = "10_available_topics.jpeg")
ggsave("10_bestseller_topics.jpeg", width = 297, height = 210, units = "mm")




####HEATMAPE with 10 main best sell topics 



################################################################################
#auf basis openRefin_tbl plotten, wo title,publischer,subtopics zusammengefügt 
################################################################################
openRefin_tbl <-  openRefin_tbl %>% rename("itemID" = "ï..itemID")

joined_tbl <- left_join(openRefin_tbl, transactions_tbl, by = "itemID")
#daten vorbereiten
joined_tbl <- joined_tbl[c(1,7:10,2:6)] 

joined_tbl
bestseller_topics <- joined_tbl %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click),
            nBasket = sum(basket), 
            nOrder = sum(order),
            N = n()) %>% 
  arrange(desc(nClick)) %>% print()
bestseller_topics <- bestseller_topics[1:50,]
bestseller_topics_joined_tbl <- left_join(bestseller_topics, openRefin_tbl, by = "itemID")
bestseller_topics <-  bestseller_topics_joined_tbl %>% select(main.topic, nClick, nBasket, nOrder)
bestseller_topics <-  bestseller_topics %>% gather(key = "Transaction", value = "Amount", "nClick":"nOrder")
bestseller_topics
#ploten#
ggplot(bestseller_topics, aes(x= Transaction, y= main.topic, fill = Amount))+ geom_tile()+scale_fill_gradient(low="blue", high="red")

################################################################################
################################################################################
log_scala_heat_mape <- bestseller_topics %>% mutate(Amount= sapply(Amount, function(x) x/10))
ggplot(log_scala_heat_mape, aes(x= Transaction, y= main.topic, fill = Amount))+
  geom_tile()+scale_fill_gradient(low="blue", high="red")+
  ylab("Most intresting topics") +
  xlab("Steps")+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  ggtitle("Heatmap: summarize by all transactions acc.to the most interesting
          topics for users") + 
  labs(fill = "Log Scale")
  
ggsave("Heatmap.jpeg", width = 297, height = 210, units = "mm")

###############################################################################
###Examples for Presentation###################################################

#joined_tbl[c(3,26,39,82,83,177),]
#items_tbl %>%  filter(str_detect(title, "mo$")) %>% select(itemID, title, author,publisher, main.topic,subtopics)

glimpse(items_tbl)
head(items_tbl, n = 20)
joined_tbl %>% 
  select(itemID, title, author, publisher) %>%
  group_by(title) %>%
  summarise(N = n()) %>%
  arrange(N) ####### nach title

nach_autor <- items_tbl %>%
  select(itemID, title, author, publisher) %>%
  group_by(author) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) ###### nach author
nach_autor <- nach_autor %>% rename("Frequency" = N)             
publisher_expl <- items_tbl %>%  filter(str_detect(publisher, "^.."))
filter(items_tbl, itemID %in% c(76427,33906,35184,53828, 51441))
publisher_expl <-  filter(items_tbl, itemID %in% c(73628,4449,41474,45331,62464))
title_ex_utf8 <- filter(items_tbl, itemID %in% c(2,18,35,74,75,91))
expl_main_t <- filter(items_tbl, main.topic == "")
items_tbl %>% arrange(publisher, desc(N))
filter(items_tbl, main.topic == "")  
expl_sub <- filter(items_tbl, subtopics == "[]")
length(expl_sub$subtopics)
#################################### ############################################

summary(transactions_tbl)
transactions_tbl %>% filter(click == 118)
transactions_tbl %>% filter(basket == 293)
transactions_tbl %>% filter(order == 28)
cnt_click <- count(transactions_tbl, click) %>% arrange(desc(click))


expl_click <-  filter(joined_tbl, click %in% c(118,97,36,34,32))
expl_basket <-  filter(joined_tbl, basket %in% c(293,287,268,182,150,119))
expl_order <- filter(joined_tbl, order %in% c(28,27,26,25,23,21))
ggplot(transactions_tbl) + geom_col(aes(x = itemID,  y = click))
length(transactions_tbl$click)


#### HEATMAP with 10 main best sell topics 

#joined_tbl %>%  select(itemID, main.topic, click, basket, order) %>%
#group_by(main.topic)%>%
bestseller_topics <- joined_oR %>%
  group_by(itemID) %>%
  summarise(nClick = sum(click),
            nBasket = sum(basket), 
            nOrder = sum(order), 
            N = n()) %>%
  arrange(desc(nOrder)) %>%
  print()

### Transaction Matrix erstellen



trans_seq <- joined_oR %>%
  group_by(sessionID) %>%
  summarize(
    SIZE = n()
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

rules <- apriori(trans, parameter = list(minlen=2, supp=0.005, conf=0.8))
inspect(rules)

rules.sorted <- sort(rules, by = "lift")
inspect(rules.sorted)
inspectDT(rules.sorted)
plot(rules.sorted, method="grouped")
