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
items_dtm2000 <- items_dtm#[1:2000, ]

#just take terms which occur in more than just one document (for similarity)
items_dtm2000 <- items_dtm2000[, doc_freq >= 2]

### cosine similarity berechnen
cos_similarity <- function(A,B) {
  oben = sum(A*B, na.rm=T)
  unten = sqrt(sum(A^2, na.rm=T)) * sqrt(sum(B^2, na.rm=T))
  result = oben/unten
  
  return(result)
}

### NA'S in items_dtm2000 durch 0 ersetzen für leichtere Berechnungen
items_dtm2000[is.na(items_dtm2000)] <- 0
### struktur von items_dtm anzeigen lassen
str(items_dtm2000) ## ???

tictoc::tic("Dauer Cosine mit proxyC und drop0 = TRUE")
parallelMap::parallelStartSocket(cpus = parallel::detectCores())

cosine_dtm <- proxyC::simil(items_dtm2000, method = "cosine", drop0 = TRUE)
#cosine_dtm <- lsa::cosine(t(as.matrix(items_dtm2000)))

parallelMap::parallelStop()
tictoc::toc()

# für spezifische items die 5 höchsten similarities ausgeben lassen
head(sort(cosine_dtm[187,], decreasing = TRUE))


