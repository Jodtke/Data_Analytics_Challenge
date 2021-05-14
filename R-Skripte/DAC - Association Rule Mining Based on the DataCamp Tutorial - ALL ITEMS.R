################################################################################

# DAC - Association Rule Mining Based on the DataCamp Tutorial

################################################################################

# Mining Association Rules based on the book's title

################################################################################

# loading libraries

library(arules)
library(arulesViz)
library(tidyverse)
#library(readxl)
library(plyr)
library(ggplot2)
library(knitr)
library(lubridate)

# data pre-processing

# read transactions into R dataframe
transactions <- read_delim("./Data/transactions.csv", delim = "|")
transactions <- transactions[complete.cases(transactions), ]
tranasctions <- transactions %>% mutate(itemID = as.factor(itemID))

# read items into R dataframe
items <- read_delim("./Data/items.csv", delim = "|")

# join transactions and items together
#complete <- left_joing(transactions, items, by = "itemID")
complete <- full_join(transactions, items, by = "itemID")

# filter for sessions, where items were purchased
#complete <- complete %>% 
#  filter(order > 0)

# select only the columns sessionID and title
#complete <- complete %>% 
#  select(sessionID, title)
complete <- complete %>% 
  select(1:9)

#Convert and edit InvoiceNo into numeric
sessionID <- as.numeric(as.character(transactions$sessionID))

# convert the dataframe into transaction data

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(complete, "sessionID",
                         function(df1)paste(df1$title,
                                            collapse = "|"))


# Problem: Es gibt viele B?cher, hinter denen ", Band 1" steht. Folglich w?rde ", Band 1" als eigenes Item betrachtet werden.
# Wir m?ssen also ein anderes Symbol zum Trennen by collapse verwenden, bspw. "|"

#set column sessionID of dataframe transactionData  
transactionData$sessionID <- NULL

#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
View(transactionData)

# This format for transaction data is called the basket format. 
# Next, you have to store this transaction data into a .csv (Comma Separated Values) file. 
# For this, write.csv()
write.csv(transactionData, "full_join_transactions.csv", quote = FALSE, row.names = FALSE)

# Next, you have to load this transaction data into an object of the transaction class.
tr <- read.transactions("full_join_transactions.csv", format = "basket", sep = "|")
# m?ssen hier bei sep das "|" Symbol verwenden, da wir diese weiter oben auch so getrennt haben!

summary(tr)
inspect(head(tr,150))

# Sieht an sich schonmal klasse aus! Problem: Hier wird suggeriert, dass "Die Welle"
# das am h?ufigsten gekaufte Buch sei. Das ist jedoch nicht unbedingt richtig,
# da "Die Welle" am h?ufigsten in den Transaktionen aufgetaucht ist, andere B?cher
# aber vielleicht mit gr??erer St?ckzahl gekauft wurden

# Create an item frequency plot for the top 10 items

library(RColorBrewer)
itemFrequencyPlot(tr, topN = 10, type = "absolute", col = brewer.pal(8, "Pastel2"), main = "Absolute Item Frequency Plot")

# Funktioniert tats?chlich da in der EDA ebenfalls diese B?cher angezeigt werden!

# Generating Rules!

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.0001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:100])
inspect(head(sort(association.rules, by = "support"), 10))

# Limiting the number and size of rules

# If you want stronger rules, you can increase the value of conf and for more extended rules give higher value to maxlen.
shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))

# Removing redundant rules

# You can remove rules that are subsets of larger rules.
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

# Finding Rules related to given items

# load the data set with the target items
evaluation <- read.csv("./Data/evaluation.csv")
testSet <- evaluation %>%
  inner_join(items, by = "itemID") %>% 
  select(title)

#testSet <- apply(head(testSet,3),1, as.character)
testSet <- as.character(head(testSet,1))

target.association.rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.8), appearance = list(default="lhs",rhs=testSet))
# Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items
inspect(head(target.association.rules))

# Similarly, to find the answer to the question Customers who bought METAL also bought.... you will keep METAL on lhs:
target.association.rules <- apriori(tr, parameter = list(supp=0.00001, conf=0.2),appearance = list(lhs="Breathtaking",default="rhs"))
inspect(target.association.rules)

# Visualizing Association Rules

# Scatter-Plot

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plot(subRules, method = "two-key plot")

plotly_arules(subRules)

top10suRules <- head(subRules, n = 10, by = "confidence")
plot(top10suRules, method = "graph", engine = "htmlwidget")

subRules <- head(subRules, n = 20, by = "lift")
plot(subRules, method = "paracoord")

################################################################################

# Mining Association Rules based on the book's main topic

################################################################################

# Jetzt das selbe Spiel, allerdings nicht mit Titel sondern Main Topic!

# join transactions and items together
complete <- left_join(transactions, items, by = "itemID")

# filter for sessions, where items were purchased
complete <- complete %>% 
  filter(order > 0)

# select only the columns sessionID and title
complete <- complete %>% 
  select(sessionID, `main topic`)

#Convert and edit InvoiceNo into numeric
sessionID <- as.numeric(as.character(transactions$sessionID))

# convert the dataframe into transaction data

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(complete, "sessionID",
                         function(df1)paste(df1$`main topic`,
                                            collapse = "|"))

#set column sessionID of dataframe transactionData  
transactionData$sessionID <- NULL

#Rename column to items
colnames(transactionData) <- c("main topics")
#Show Dataframe transactionData
transactionData

# This format for transaction data is called the basket format. 
# Next, you have to store this transaction data into a .csv (Comma Separated Values) file. 
# For this, write.csv()
write.csv(transactionData, "full_join_transactions.csv", quote = FALSE, row.names = FALSE)

# Next, you have to load this transaction data into an object of the transaction class.
tr <- read.transactions("full_join_transactions.csv", format = "basket", sep = "|")
# m?ssen hier bei sep das "|" Symbol verwenden, da wir diese weiter oben auch so getrennt haben!

summary(tr)

# Create an item frequency plot for the top 10 items

library(RColorBrewer)
itemFrequencyPlot(tr, topN = 10, type = "absolute", col = brewer.pal(8, "Pastel2"), main = "Absolute Item Frequency Plot")

transactions %>% 
  left_join(items, by = "itemID") %>% 
  filter(order > 0) %>% 
  group_by(`main topic`) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

# Funktioniert nicht wie bei title, da die EDA mehr und andere Topics anzeigt

# Generating Rules!

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.0001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:10])

# Removing redundant rules

# You can remove rules that are subsets of larger rules.
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plot(subRules, method = "two-key plot")

plotly_arules(subRules)

top10suRules <- head(subRules, n = 10, by = "confidence")
plot(top10suRules, method = "graph", engine = "htmlwidget")

subRules <- head(subRules, n = 20, by = "lift")
plot(subRules, method = "paracoord")

################################################################################