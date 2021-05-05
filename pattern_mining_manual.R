############### Pattern Mining - Basic ###############
# generate some random names
names = c(
  "John Smith", 
  "Wigberht Ernust",
  "Samir Henning",
  "Everette Arron",
  "Erik Conor",
  "Smith J",
  "Smith John",
  "John S",
  "John Sally"
);

# split those names and get all ways to write that name
split_names = lapply(
  X = names,
  FUN = function(x){
    print(x);
    # split by a space
    c_split = unlist(x = strsplit(x = x, split = " "));
    # get both combinations of c_split to compensate for order
    c_splits = list(c_split, rev(x = c_split));
    # return c_splits
    c_splits;
  }
)

# suppose we're looking for John Smith
search_for = "John Smith";

# split it by " " and then find all ways to write that name
search_for_split = unlist(x = strsplit(x = x, split = " "));
search_for_split = list(search_for_split, rev(x = search_for_split));

# initialise a vector containing if search_for was matched in names
match_statuses = c();

# for each name that's been split
for(i in 1:length(x = names)){
  
  # the match status for the current name
  match_status = FALSE;
  
  # the current split name
  c_split_name = split_names[[i]];
  
  # for each element in search_for_split
  for(j in 1:length(x = search_for_split)){
    
    # the current combination of name
    c_search_for_split_names = search_for_split[[j]];
    
    # for each element in c_split_name
    for(k in 1:length(x = c_split_name)){
      
      # the current combination of current split name
      c_c_split_name = c_split_name[[k]];
      
      # if there's a match, or the length of grep (a pattern finding function is
      # greater than zero)
      if(
        # is c_search_for_split_names first element in c_c_split_name first
        # element
        length(
          x = grep(
            pattern = c_search_for_split_names[1],
            x = c_c_split_name[1]
          )
        ) > 0 &&
        # is c_search_for_split_names second element in c_c_split_name second 
        # element
        length(
          x = grep(
            pattern = c_search_for_split_names[2],
            x = c_c_split_name[2]
          )
        ) > 0 ||
        # or, is c_c_split_name first element in c_search_for_split_names first 
        # element
        length(
          x = grep(
            pattern = c_c_split_name[1],
            x = c_search_for_split_names[1]
          )
        ) > 0 &&
        # is c_c_split_name second element in c_search_for_split_names second 
        # element
        length(
          x = grep(
            pattern = c_c_split_name[2],
            x = c_search_for_split_names[2]
          )
        ) > 0
      ){
        # if this is the case, update match status to TRUE
        match_status = TRUE;
      } else {
        # otherwise, don't update match status
      }
    }
  }
  
  # append match_status to the match_statuses list
  match_statuses = c(match_statuses, match_status);
}

search_for;

# [1] "John Smith"

cbind(names, match_statuses);

# names             match_statuses
# [1,] "John Smith"      "TRUE"        
# [2,] "Wigberht Ernust" "FALSE"       
# [3,] "Samir Henning"   "FALSE"       
# [4,] "Everette Arron"  "FALSE"       
# [5,] "Erik Conor"      "FALSE"       
# [6,] "Smith J"         "TRUE"        
# [7,] "Smith John"      "TRUE"        
# [8,] "John S"          "TRUE"
# [9,] "John Sally"      "FALSE"

################## Pattern Mining - Advanced ############
# libraries
library(gtools)
x <- c("Yoda","speaks","thus")
permutations(n=3, r=3, v=x, repeats.allowed = FALSE) # n=num.elems r=num.times v=x

# generate some random names
names <- c(
  "John Smith", 
  "Robert Allen Zimmerman (Bob Dylan)",
  "Everette Camille Arron",
  "Valentina Riquelme Molina",
  "Smith J",
  "Smith John",
  "John S",
  "John Sally"
);

# drop parentheses, if any
names <- gsub("[(|)]", "", names)


# split those names and get all ways to write that name into a list of same length
split_names <- lapply(
  X = gsub("[(|)]", "", names),
  FUN = function(x){
    print(x);
    # split by a space
    c_split = unlist(x = strsplit(x = x, split = " "));
    # get all permutations of c_split to compensate for order
    n <- r <- length(c_split)
    c_splits <- list(permutations(n=n, r=r, v=c_split, repeats.allowed = FALSE))
    # return c_splits
    c_splits;
  }
)

split_names

# suppose we're looking for this name
search_for <- "Bob Dylan";

# split it by " " and then find all ways to write that name
search_for_split <- unlist(x = strsplit(x = search_for, split = " "));
# permutations over search_for_split seem redundant

# initialize a vector containing if search_for was matched in names
match_statuses <- c();

# for each name that's been split
for(i in 1:length(names)){
  
  # the match status for the current name
  match_status <- FALSE;
  
  # the current split name
  c_split_name <- as.data.frame(split_names[[i]]);
  
  # for each element in c_split_name
  for(j in 1:nrow(c_split_name)){
    
    # the current permutation of current split name
    c_c_split_name <- as.matrix(c_split_name[j,]);
    
    # will receive hits in name's words, one by one, in sequence
    hits <- rep(0, 20) # length 20 should always be above max number of words in names
    
    # for each element in search_for_split
    for(k in 1:length(search_for_split)){
      
      # the current permutation of name
      c_search_for_split <- search_for_split[[k]];
      
      # L first hits will receive hit counts
      L <- min(ncol(c_c_split_name), length(search_for_split));
      
      # will match as many words as the shortest current pair of names  
      for(l in 1:L){
        
        # if there's a match, the length of grep is greater than zero
        if(
          # is c_search_for_split in c_c_split_name's lth element
          length(
            grep(
              pattern = c_search_for_split,
              x = as.character(c_c_split_name[l])
            )
          ) > 0 ||
          # or, is c_c_split_name's lth element in c_search_for_split
          length(
            grep(
              pattern = c_c_split_name[l],
              x = c_search_for_split
            )
          ) > 0
          
          # if this is the case, record a hit    
        ){
          hits[l] <- 1;
        } else {
          # otherwise, don't update hit
        }
      }
    }
    
    # take L first elements
    hits <- hits[1:L]
    
    # if hits vector has all ones for this permutation, update match status to TRUE
    if(
      sum(hits)/length(hits)==1 # <- can/should be made more flexible (agrep, or sum/length<1)
    ){
      match_status <- TRUE;
    } else {
      # otherwise, don't update match status
    }
  }
  
  # append match_status to the match_statuses list
  match_statuses <- c(match_statuses, match_status);
}

search_for;

cbind(names, match_statuses);
#
#
#