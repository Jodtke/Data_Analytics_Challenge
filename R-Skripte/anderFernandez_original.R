url = "http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip"
download.file(url, destfile = "data.zip")
dir.create("data")
unzip("data.zip",exdir = "data")   

files = paste0("data/",list.files("data"))

ratings = read.csv(files[1], sep = ";")
books = read.csv(files[2], sep = ";")
users = read.csv(files[3], sep = ";")

# categories
set.seed(1234)
categories = c("Action and Adventure","Classic","Detective and Mystery","Fantasy")
books$category = sample( categories, nrow(books), replace=TRUE, prob=c(0.25, 0.3, 0.25, 0.20))
books$category = as.factor(books$category)

# isbn anhängen
books$ISBN = paste0("Isbn.",books$ISBN)
books$User.ID = paste0("User.",books$User.ID)
users$ISBN = paste0("Isbn.", users$ISBN)
#ratings$ISBN = paste0("Isbn.",books$ISBN)
#ratings$User.ID = paste0("User.",books$User.ID)

# plot
books %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10) 

# non-zero ratings
books_ratings = books[books$Book.Rating!= 0, ]

# plot2
books_ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)

# scores für die person
ratings_sum = books_ratings %>%
  group_by(User.ID) %>%
  count() 
summary(ratings_sum$n)

# nur ausreichend aussagekräftige user nutzen
user_index = ratings_sum$User.ID[ratings_sum$n>4]

users = users[users$User.ID %in% user_index, ]
ratings = ratings[ratings$User.ID %in% user_index, ]
books = books[books$ISBN %in% ratings$ISBN,]

rm(ratings_sum, user_index)


