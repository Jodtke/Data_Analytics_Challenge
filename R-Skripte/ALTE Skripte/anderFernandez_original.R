library(tidyverse)

url = "http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip"
download.file(url, destfile = "data.zip")
dir.create("data")
unzip("data.zip",exdir = "data")   

files = paste0("data/",list.files("data"))

ratings = read.csv(files[2], sep = ";")
books = read.csv(files[3], sep = ";")
users = read.csv(files[4], sep = ";")

# categories
set.seed(1234)
categories = c("Action and Adventure","Classic","Detective and Mystery","Fantasy")
books$category = sample( categories, nrow(books), replace=TRUE, prob=c(0.25, 0.3, 0.25, 0.20))
books$category = as.factor(books$category)

# isbn anhängen
books$ISBN = paste0("Isbn.",books$ISBN)
users$User.ID = paste0("User.",users$User.ID)
#users$ISBN = paste0("Isbn.", users$ISBN)
ratings$ISBN = paste0("Isbn.",ratings$ISBN)
ratings$User.ID = paste0("User.",ratings$User.ID)
# Übersicht
glimpse(books)
glimpse(users)
glimpse(ratings)

# plot
ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10) 

# non-zero ratings
ratings = ratings[ratings$Book.Rating!= 0, ]

# plot2
ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)

# scores für die person
ratings_sum = ratings %>%
  group_by(User.ID) %>%
  count() 
summary(ratings_sum$n)

# nur ausreichend aussagekräftige user nutzen
user_index = ratings_sum$User.ID[ratings_sum$n>4]

users = users[users$User.ID %in% user_index, ] # ACHTUNG: FÜR USER IN 'BOOKS' SUCHEN!
ratings = ratings[ratings$User.ID %in% user_index, ]
books = books[books$ISBN %in% ratings$ISBN, ]

rm(ratings_sum, user_index)

####### content-based filterung: item-contents ###########
library(cluster)
#books_distance = books[,c("ISBN","Book.Author","Publisher")] 
#dissimilarity = daisy(books_distance, metric = "gower")
book_feature <- books[1:10000, c("Book.Author", "Publisher", "category")]
book_feature <- book_feature %>%
  mutate(Book.Author = as.factor(Book.Author),
         Publisher = as.factor(Publisher),
         category = as.factor(category))
dissimilarity_matrix <- daisy(data.frame(book_feature), metric = "gower", weights = c(2,0.5,1))
dissimilarity_matrix = as.matrix(dissimilarity_matrix)
row.names(dissimilarity_matrix) <- books$ISBN[1:10000]
colnames(dissimilarity_matrix) <- books$ISBN[1:10000]

dissimilarity_matrix[15:20,15:20]

# spezielle UerID suchen
user_id = "User.167812"

user_books = ratings %>%
  filter(User.ID == user_id & ISBN %in% books$ISBN[1:10000]) %>%
  arrange(desc(Book.Rating))

head(user_books,10)
glimpse(user_books)
########### FUNKTION ##############
books$ISBN = as.character(books$ISBN)
selected_books = user_books[ ,c("ISBN", "Book.Rating")]
glimpse(selected_books)

recomendar = function(selected_books, dissimilarityMatrix, books, n_recommendations = 5)
  {
  
  selected_book_indexes = which(colnames(dissimilarityMatrix) %in% selected_books$ISBN)
  
  
  results = data.frame(dissimilarityMatrix[, selected_book_indexes], 
                       recommended_book = row.names(dissimilarityMatrix),
                       stringsAsFactors = FALSE)
  recomendaciones = results %>%
    pivot_longer(cols = c(-"recommended_book") , names_to = "read_book", 
                 values_to = "dissimilarity") %>%
    left_join(selected_books, by = c("recommended_book" = "ISBN"))%>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_book != readed_book) %>%
    filter(!is.na(Book.Rating) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * Book.Rating) %>%
    arrange(desc(weighted_score)) %>%
    filter(weighted_score>0) %>%
    group_by(recommended_book) %>% slice(1) %>%
    top_n(n_recommendations, weighted_score) %>%
    left_join(books, by = c("recommended_book"="ISBN")) %>%
    arrange(desc(weighted_score))
  
  return(recomendaciones)
}

recomendaciones = recomendar(selected_books, dissimilarity_matrix, books)
recomendaciones # FUNKTIONIERT!!!

############## Visualisierung ################
library(png)
library(gtable)
visualizar_recomendacion = function(recomendation, recommended_book, image, n_books = 5)
  {
  
  if(n_books > nrow(recomendation)) {n_books = nrow(recomendation)}
  
  plot = list()
  
  dir.create("content_recommended_images")
  for(i in 1:n_books){
    # Create dir & Download the images
    img = pull(recomendation[i,which(colnames(recomendation) == image)])
    name = paste0("content_recommended_images/",i,".jpg")
    suppressMessages(
      download.file(as.character(img), destfile = name ,mode = "wb") 
    )
    
    # Assign Objetc
    plot[[i]] = rasterGrob(readJPEG(name))
  }
    
  o.call(marrangeGrob, args = list(plot, ncol = n_books, nrow = 1, top=""))
  
}

visualizar_recomendacion(recomendaciones, "recommended_book","Image.URL.M") ### FUNKTIONIERT NICHT!

######## collaborative filtering: item-based recommendation system ##########


