library(tidyverse)
library(quanteda)

getwd()
#setwd("/Users/sebastianlocher/Desktop/DCA")

items <- read_csv(file="./Data/items6.csv", col_names=T, col_types=cols(
  itemID=col_factor(),
  title=col_factor(),
  author=col_factor(),
  publisher=col_factor(),
  main.topic=col_factor(),
  subtopics=col_character()
))
glimpse(items)

# sonderzeichen entfernen
items$subtopics <- gsub("\\[|\\]", "", items$subtopics)
items$subtopics <- gsub(","," ",items$subtopics)

items$subtopics[items$subtopics == ""] <- NA
sum(is.na(items$subtopics))
head(items, n=20)

item_toks <- tokens(items$subtopics)
items_dtm <- dfm(x=item_toks, verbose=quanteda_options("verbose"))
#document frequency per term
doc_freq <- docfreq(items_dtm)
items_dtm2000 <- items_dtm[1:2000, ]

#just take terms which occur in more than just one document (for similarity)
items_dtm2000 <- items_dtm2000[, doc_freq >= 2]

### cosine similarity berechnen
cos_similarity <- function(A,B) {
  num = sum(A*B, na.rm=T)
  den = sqrt(sum(A^2, na.rm=T)) * sqrt(sum(B^2, na.rm=T))
  result = num/den
  
  return(result)
}

cosine_dtm <- cosine(t(as.matrix(items_dtm2000)))
head(sort(cosine_dtm[177,], decreasing = TRUE))

