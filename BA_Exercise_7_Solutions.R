#-------------------------------------------------------------------------------
# Business Analytics - Summer Term 2019 - Exercise 7: Association Rules 
# by John Vicente (Catholic University Eichstaett-Ingolstadt)
#-------------------------------------------------------------------------------

# Download libraries needed for this session.

library(arules)
library(arulesViz)

# Load the weather and Groceries dataset.

weather <- read.table("./Data/weather.txt", header=T)
data("Groceries")

#-------------------------------------------------------------------------------
# Association Rule Mining (weather dataset)
#-------------------------------------------------------------------------------

# Transform the data frame weather into the tabular format. Set up a matrix called
# weather_tidy that contains 14 rows. The amount of columns should be the same
# as the amount of unique values in the weather data frame. Label the columns
# appropriately.

unique_values <- unique(as.vector(as.matrix(weather)))
weather_tidy <- matrix(nrow = 14, ncol = length(unique_values))
colnames(weather_tidy) <- unique_values

# Replace the columns with the respective binary values that indicate whether
# the condition is true or not.

weather_tidy[,"Sunny"] <- ifelse(weather$outlook == "Sunny", 1, 0)
weather_tidy[,"Overcast"] <- ifelse(weather$outlook == "Overcast", 1, 0)
weather_tidy[,"Rain"] <- ifelse(weather$outlook == "Rain", 1, 0)
weather_tidy[,"Hot"] <- ifelse(weather$temperature == "Hot", 1, 0)
weather_tidy[,"Mild"] <- ifelse(weather$temperature == "Mild", 1, 0)
weather_tidy[,"Cool"] <- ifelse(weather$temperature == "Cool", 1, 0)
weather_tidy[,"High"] <- ifelse(weather$humidity == "High", 1, 0)
weather_tidy[,"Medium"] <- ifelse(weather$humidity == "Medium", 1, 0)
weather_tidy[,"False"] <- ifelse(weather$windy == "False", 1, 0)
weather_tidy[,"True"] <- ifelse(weather$windy == "True", 1, 0)
weather_tidy[,"No"] <- ifelse(weather$play == "No", 1, 0)
weather_tidy[,"Yes"] <- ifelse(weather$play == "Yes", 1, 0)

# Apply the function apriori() on the matrix weather_tidy. Set the support to 0.01
# and the confidence to 0.5. Assign the results to an object called rules. How
# many rules do we have mined?

rules_1 <- apriori(weather_tidy, parameter = list(support = 0.01, confidence = 0.5))
summary(rules_1) # 568 rules

# Plot the rules by wrapping the rules object into the plot function. 
# Interpret the plot

plot(rules_1)

# Have a look into the top 5 rules ordered by lift.

inspect(head(sort(rules_1, by = "lift"), 5))

# Create two subset of rules that have play = no and play = yes on the right
# hand side.

rules_1_rhs_yes <- subset(rules_1, subset = rhs %in% "Yes")
rules_1_rhs_no <- subset(rules_1, subset = rhs %in% "No")

# Show the top 5 rules of each subset ordered by support. Make use of the inspect() 
# function. Interpret support, confidence and lift.

inspect(head(sort(rules_1_rhs_yes, by = "support"), 5))
# -> Rule 2: Support: Rule is observed in 42.86% of all cases
#            Confidence: P(Yes|Medium) = 85.71%
#            Lift: When there is normal humidity people are 1.33 times as
#                  likely to play as people from the entire data set.

inspect(head(sort(rules_1_rhs_no, by = "support"), 5))
# -> Rule 1: Support: Rule is observed in 28.57% of all cases
#            Confidence: P(No|High) = 57.14%
#            Lift: When there is high humidity people are 1.6 times as
#                  likely not to play as people from the entire data set.


# TASK: Apply the apriori algorithm using the confidence threshold of 0.7 and store
# the results to an object called rules_2. Again create two subsets that divide
# the rules. How many rules do we have mined?

rules_2 <- apriori(weather_tidy, parameter = list(support = 0.01, confidence = 0.7))
summary(rules_2) # 252 rules

# TASK: Again, create two subset of rules that have play = no and play = yes on the right
# hand side.

rules_2_rhs_yes <- subset(rules_2, subset = rhs %in% "Yes")
rules_2_rhs_no <- subset(rules_2, subset = rhs %in% "No")

# TASK: Show the top 5 results ordered by confidence. Why are the lift values mined
# from the subset play = no higher than the ones from play = yes?

inspect(head(sort(rules_2_rhs_yes, by = "confidence"), 5))
inspect(head(sort(rules_2_rhs_no, by = "confidence"), 5))


#-------------------------------------------------------------------------------
# Association Rule Mining (Groceries dataset)
#-------------------------------------------------------------------------------

# Have a look into the Groceries object by applying the summary() function. How
# many transactions and itemsets are available in the dataset? 

summary(Groceries)

# Which items were bought by the first five observations? How many items have they
# bought? 

inspect(Groceries[1:5])
size(Groceries[1:5])

# Plot a histogram of the amount of items bought per transaction. Interpret the
# histogram.

hist(size(Groceries)) 

# Which observations bought more than 25 items in their transaction? Investigate
# one of these observations and show the items the person has bought.

which(size(Groceries) > 25) 
inspect(Groceries[1092])

# Apply the itemFrequencyPlot() function to plot the itemset frequencies of all
# items that have a relative frequency of at least 5%.

itemFrequencyPlot(Groceries, support = 0.05) 

# Apply the apriori algorithm using the support threshold of 0.001 and the 
# confidence threshold of 0.2 and store the results to an object called g_rules.

g_rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.2))

# TASK: Inspect the top 3 association rules ordered by lift.

inspect(head(sort(g_rules, by = "lift")))

# TASK: Interpret the lift value of the first association rule

#       When people buy bottled beer and red/blush wine the people are 35.71 times
#       as likely to buy liquor as people from the entire dataset

# TASK: Create a subset of rules that contains only "beef" on the right hand side.
# Show the first 5 rules ordered by confidence and interpret support, confidence
# and lift of the first rule.

g_rules_beef <- subset(g_rules, rhs %in% "beef")
inspect(head(sort(g_rules_beef, by = "confidence"), 5))

# -> Rule 1: Support: Rule is observed in 0.11% of all cases
#            Confidence: P(beef|lhs) = 55%
#            Lift: When the conditions on the lhs are met people are 10.48 times as
#                  likely to buy beef as people from the entire data set.
plot(g_rules_beef, method = "graph")



