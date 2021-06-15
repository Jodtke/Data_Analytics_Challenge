############ Data Mining Cup ##########

###
##Rules with sessionID
## Packages
library(tidyverse)

## Arbeitsverzeichnis
getwd()

## Daten
items <- read_csv(file="./Data/items6.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_factor(),
  author=col_factor(),
  publisher=col_factor(),
  main.topic=col_factor(),
  subtopics=col_character()
))
#head(items, n=20)
#glimpse(items)

trans <- read_delim(file="./Data/transactions.csv", col_names=T, delim="|", col_types=cols(
  sessionID = col_factor(),
  itemID = col_factor(),
  click = col_integer(),
  basket = col_integer(),
  order = col_integer()
))
## joinen & Reihenfolge der Spalten verändern
joined_item_trans <- left_join(trans, items, by="itemID")
joined_item_trans <- joined_item_trans[, c(7,1,8:10,2:6) ]
#head(joined_item_trans, n=10)
transactions_raw <- read.csv(file = "./Data/transactions.csv", header = T, sep = "|", quote = "", row.names = NULL, stringsAsFactors = F)
#dim(transactions_raw)  #365 143 x 5
openRefine <- read.csv(file = "./Data/items6.csv", header = T, sep = ",", row.names = NULL, stringsAsFactors = F, encoding = "UTF-8")
#dim(openRefine) #78334 X 6
evaluation <-  read.csv(file = "./Data/evaluation.csv", header = T, quote = "", row.names = NULL, stringsAsFactors = F)
#dim(evaluation)

## als Tibble kovertieren
transactions_tbl <- as_tibble(transactions_raw)
#dim(transactions_tbl)
oR_tbl <- as_tibble(openRefine)
#dim(oR_tbl)
evaluation_tbl <- as_tibble(evaluation)

#join the files
joined_oR <- left_join(transactions_tbl,oR_tbl, by = "itemID")  #365143 

#abspeichern nur die sessions, die >= 2 Bücher hatten
dup.ses <- joined_oR[joined_oR$sessionID %in%
                       joined_oR$sessionID[duplicated(joined_oR$sessionID)],] # das sind knapp 183k sessions

length(unique(dup.ses$itemID)) #für 14471 Bücher würde das funktionieren

##prüfen wie viel Bücher von denen, die in transaction DS drin sind, auch in evaluation DS erscheinen.
#Ob das Sinn macht das Ganze zu basteln und zu implimentieren 
#pruff_ses <- right_join(dup.ses, evaluation_tbl, by = "itemID")
#view(pruff_ses %>% group_by(itemID))
#length(unique(pruff_ses$itemID))
#pruff_ses <- drop_na(pruff_ses)  
#pruff_ses %>% group_by(itemID) %>% summarise(n=n()) # 304 Bücher in DS evaluation
##############################################
#dup.ses %>% filter(itemID==52474)
#z.b. wir suchen Recomendation für item_44827
# item <- 52474
# this_ses <- dup.ses %>% group_by(sessionID) %>%    #aus dem Datensatz, wo nur dupplicated sessions drin
#             filter(itemID==item) %>% select(sessionID)
# this_ses <- this_ses$sessionID
# this_books_tbl <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID) 
# #this_books <- this_books %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
#               #summarise(n=n()) %>% arrange(desc(n)) %>% filter(n == max(n))
# #
# this_books_tbl
# amount_ofbooks <- this_books_tbl %>% select(sessionID,itemID) %>% group_by(sessionID) %>%
#                    summarise(n=n()) %>% arrange(desc(n))
# amount_ofbooks
# top5 <- this_books_tbl %>% group_by(itemID) %>% filter(itemID != item)%>% 
#         summarise(nClick=sum(click),nBasket=sum(basket),norder=sum(order)) %>%
#         arrange(desc(nClick))
# top5 <- slice_head(top5, n = 5)
# top5 <- top5$itemID
# top5


#Recommendation Function based on sessionID
 
Recommendation_function <- function(activeItem){
  this_ses <- dup.ses %>% group_by(sessionID) %>%   
    filter(itemID==activeItem) %>% select(sessionID)
  if (nrow(this_ses) == 0){
    return(list())
  }
  this_ses <- this_ses$sessionID
  this_books_tbl <- dup.ses %>% filter(sessionID%in%this_ses) %>% arrange(sessionID) 
  potential_recommendations <- this_books_tbl %>% group_by(itemID) %>% filter(itemID != activeItem)%>% 
    summarise(nClick=sum(click),nBasket=sum(basket),norder=sum(order)) %>%
    arrange(desc(nClick))
  alpha <- 0.2
  beta <- 0.3
  gamma <-0.5
  potential_recommendations <- potential_recommendations %>% 
    mutate(kennzahl = (nClick*alpha + nBasket*beta + norder*gamma)/sum(alpha,beta,gamma))
  potential_recommendations <- potential_recommendations %>% select(itemID,kennzahl) %>% arrange(desc(kennzahl)) %>%
    filter(itemID != activeItem) %>% slice_max(kennzahl,n=10) #gibt mehr als 10 raus, wegen gleichen Werten 
  list_of_poten_rec <- as.list(potential_recommendations) 
  return(list_of_poten_rec)
}

#try
Recommendation_function(76465)
###############################################################################

#lassen die Funktion für ein Vektor der Bücher laufen
# books <- c(5468,44827)
# result <- list()
# for (i in 1:length(books)){
#   book <- books[i]
#     result[[i]] <- Recommendation_function(book)
# }
# result

################################################################################
###09.06.2021#############Umformung der Funktion für eine Liste#################
######try for random sample of books from OR_tibble#####
set.seed(123)
test_random <- sample_n(oR_tbl, 1)    #tibble 25 rows
#test_random <- oR_tbl %>% filter(itemID==76465)
books <- as.list(test_random$itemID)   
result <- list()
for (i in 1:length(books)){
  book <- books[[i]]
  result[[i]] <- Recommendation_function(book)
  names(result)[i] <- book
}
result
#item 41198 kam tatsächlich in 207 sessions vor, deswegen 409+ potential recommendation

##linke Teil des Systems
#Fall: Das Buch hat KEINE TRANSACTIONS DATEN ###
#sollte noch überpruft werden, ob das Buch über TM Teil potential Recommendations hat#

#für ein Buch 

# recomfor <- 18797  # 18797 by the way, Spanish ! 
# selected_features <- oR_tbl %>% filter(itemID==recomfor) %>%
#   select(itemID,author,main.topic,publisher)    #nehmen von OR_tbl ausgewählte Spalten
# this_author <- selected_features$author         #als chr darstellen 
# this_publisher <- selected_features$publisher
# this_genre <- selected_features$main.topic
# auth_select <- oR_tbl %>% filter(author==this_author | publisher ==this_publisher & main.topic == this_genre)#filter einsetzen
# view(auth_select)

#könnten noch theoretish nach main.topic filtern, aber wird die Anzahl schon wieder viel zu groß.
#als Erweiterung kann man das merken !# noch dazu main.topic==this_genre| Dann sollte man igwie
#matches gewichten, abhängig davon was uns wichtig ist. Z-b- author==this_author
#(author von dem activen Buch) ist am wichtigsten, 0.5, wenn
#noch publischer übereinstimmt +0.3, wenn topic gleich ist dann +0.2. In dem Fall gibt's keine Bücher mehr
#in dem author == this_author, also nach publischer zu filtern ist ein guter Punkt, weil die Sprache dann
#auch passt!


###Funktion#####
activeItem <- 48800
Notausgang_function <- function(activeItem){
  selected_features <- oR_tbl %>% filter(itemID==7892) %>%
    select(itemID,author,main.topic,publisher)    #nehmen von OR_tbl ausgewählte Spalten
  if (nrow(selected_features) == 0){
    return(list())
  }
  this_author <- selected_features$author         #als chr darstellen 
  this_publisher <- selected_features$publisher
  this_genre <- selected_features$main.topic
  items_select <- oR_tbl %>% filter(author==this_author | publisher==this_publisher & main.topic==this_genre)#filter einsetzen
  nimm_5 <- sample_n(items_select, 5)              #nicht sicher, ob es ne gute Idee ist, die Zeile
  nimm_5 <- nimm_5$itemID   #nimmt einfach 5 random Bücher mit dem gleichen (Publisher oder author) und main.topic
  return(nimm_5)
}
Notausgang_function(7892)    #aber egtl muss sie eine Liste ausspucken

####
#über Liste laufen lassen und Lüchen ausfüllen, wo leere Liste damals war ##
for (i in 1:length(result)){
  if (length(result[[i]]) < 1){
    book <- names(result)[i]
    result[[i]] <- Notausgang_function(book)
  }
}
result
################################################################################
#simulate recommendation for ActiveItem # 76465
TM_Recommendations <- result

##result 2 is output from Simularity, die wir von Subtopics berechnet haben, hier hypotesisch
activeItem <- 76465
result2 <- list(c(34371, 66167, 35154, 35726, 36013, 37525, 50382, 50951))
names(result2) <- activeItem
result2

Subt_TM_Recommend <- result2

#result3 is output die Bücher die zusammen in sessions waren, auch hypotesisch 
result3 <- list(c(39430, 66167, 255,  1421,  4077))
names(result3) <- activeItem
result3

Transact_Recommend <- result3

#merge all potential recommendatons into one list #unter einem Name ist active item und unten alle potential recommend
all_potential_recom <- Map(c, TM_Recommendations, Subt_TM_Recommend,Transact_Recommend) 

#TM_Recommendations output vergleich mit Subt_TM_Recommend output und Transact_Recommend output

count_table <- as.data.frame(table(all_potential_recom))
count_table %>% select(all_potential_recom, Freq) %>% arrange(desc(Freq))


################################################################################
#für die Bedunung müssen die ganze Zeile von active Item extrahieren#
activeItem <- 73018
activeItem <- totalInfo %>% filter(itemID==activeItem)%>% select(itemID,uniteTopics,Beschreibung,inDupSes)

proveSubtopics <- activeItem %>% select(uniteTopics) %>% is.na()
proveKlappentext <- activeItem %>% select(Beschreibung) %>% is.na()

##Struktur von finalen Funktion

#Fall_1: Subtopic + Klappentext + inDupSes =alle sind True
if (proveSubtopics == FALSE && proveKlappentext == FALSE && activeItem$inDupSes == TRUE){
  print("fall_1")
}

#Fall_2: Subtopic + inDupSes .   kein Klappentext == NA 
if(proveSubtopics == FALSE && proveKlappentext == TRUE && activeItem$inDupSes == TRUE){
  print("Fall_2")
}

#Fall_3:Subtopic + Klappentext. Nicht in dup.ses.
if (proveSubtopics == FALSE && proveKlappentext == FALSE && activeItem$inDupSes == FALSE){
  print("fall_3")
}
#Fall_4: Nur Subtopic Simularity, kein KT, nicht in Dup.ses.   #Obwohl ich bin mir nicht sicher, ob dieser Fall kommt oder soll gleich NOTAUSGANGFUNKTION angeschaltet werden
if (proveSubtopics == FALSE && proveKlappentext == TRUE && activeItem$inDupSes == FALSE){
  print("fall_3")
else {
  Notausgang_function(activeItem)
}
  