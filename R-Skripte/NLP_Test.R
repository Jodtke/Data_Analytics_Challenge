### NLP Probe Code
library(tm)
data("crude")
crude <- as.VCorpus(crude)
crude <- tm_map(crude, stripWhitespace)
crude <- tm_map(crude, removePunctuation)
crude <- tm_map(crude, content_transformer(tolower))
crude <- tm_map(crude, removeWords, stopwords("english"))
crude <- tm_map(crude, stemDocument)



# change 3 into whatever you need for a differenct n-gram
NLPtrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

tdm_NLP <- TermDocumentMatrix(crude, control=list(tokenize = NLPtrigramTokenizer))

inspect(tdm_NLP)