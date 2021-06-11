getwd()
setwd("/Users/sebastianlocher/Desktop/DCA")

items <- read.csv("items_new.csv", sep = ";")
items$subtopics <- gsub("\\[|\\]", "", items$subtopics)
items$subtopics <- gsub(","," ",items$subtopics)

items$subtopics[items$subtopics == ""] <- NA
sum(is.na(items$subtopics))

toks <- tokens(items$subtopics)
dtm <- dfm(toks)
#document frequency per term
doc_freq <- docfreq(dtm)
dtm <- dtm[1:2000, ]

#just take terms which occur in more than just one document (for similarity)
dtm <- dtm[, doc_freq >= 2]

cosine_dtm <- cosine(t(as.matrix(dtm)))
head(sort(cosine_dtm[177,], decreasing = TRUE))





